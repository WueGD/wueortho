package wueortho.pipeline

import io.circe.*
import io.circe.syntax.*
import cats.syntax.traverse.*

import scala.compiletime.*
import scala.reflect.ClassTag
import wueortho.util.RunningTime

case class WithTags[ITags <: Tuple, S](step: S, tag: Option[String], iTags: Map[Tuple.Union[ITags], String]):
  def mkTag                                 = StepUtils.resolve(tag)
  def mkITag[K <: Tuple.Union[ITags]](k: K) = StepUtils.resolve(iTags.get(k))
  def stepName                              = step.getClass().getSimpleName().nn

object WithTags:
  def only[S, T <: Tuple](s: S) = WithTags[T, S](s, None, Map.empty)

/** Provides details for executing a pipeline step.
  *
  * Remarks for json en-/decoding:
  *   - the simple name of S must be unique
  *   - the encoding of S must not have fields that are also tags in ITags
  *   - the fields `type` and `tag` are reserved
  */
trait StepImpl[S: Encoder.AsObject: Decoder: ClassTag]:
  type ITags <: Tuple

  def tags: List[String]

  def runToStage(s: WithTags[ITags, S], cache: StageCache): Either[String, RunningTime.Measured[?]]

  def helpText: String

  def stepName: String = summon[ClassTag[S]].runtimeClass.getSimpleName().nn

  def codec: Codec[WithTags[ITags, S]] = Codec.from(taggedDec, taggedEnc)

  def taggedEnc(using enc: Encoder.AsObject[S]): Encoder[WithTags[ITags, S]] =
    Encoder.AsObject.instance[WithTags[ITags, S]]: swt =>
      val more = List("type" -> swt.step.getClass.getSimpleName.nn.asJson, "tag" -> swt.tag.asJson)
        ++ swt.iTags.map((tag, value) => tag.toString -> value.asJson)
      more.foldLeft(enc.encodeObject(swt.step))(_.add.tupled(_))

  def taggedDec(using Decoder[S]): Decoder[WithTags[ITags, S]] =
    def decodeTags(json: JsonObject) =
      tags.traverse(tag => json(tag).traverse(_.as[String]).map(_.map(tag -> _)))
        .map(_.flatten.toMap.asInstanceOf[Map[Tuple.Union[ITags], String]]).toTry

    for
      s    <- Decoder[S]
      _    <- Decoder[String].at("type").emap: tpe =>
                if tpe == s.getClass.getSimpleName.nn then Right(())
                else Left(s"expected type to be ${s.getClass.getSimpleName}")
      tag  <- Decoder[Option[String]].at("tag")
      tags <- Decoder.decodeJsonObject.emapTry(decodeTags)
    yield WithTags(s, tag, tags)
  end taggedDec

  transparent inline def deriveTags[T <: Tuple]: List[Any] = constValueTuple[T].toList
end StepImpl

object StepImpl:
  type Aux[S, X <: Tuple] = StepImpl[S] { type ITags = X }

  def apply[T](using s: StepImpl[T]) = s

  transparent inline def allImpls[T](using m: scala.deriving.Mirror.SumOf[T]): List[Any] =
    summonAll[Tuple.Map[m.MirroredElemTypes, [z] =>> StepImpl[z]]].toList
end StepImpl

object StepUtils:
  def resolve(t: Option[String]) = t.getOrElse("default")

  extension [T, R](eth: Either[T, Unit])
    def unit: Either[T, RunningTime.Measured[Unit]] = eth.map(_ => RunningTime.unit)
