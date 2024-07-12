// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.pipeline

import wueortho.util.RunningTime

import io.circe.*
import io.circe.syntax.*
import cats.syntax.traverse.*

import scala.compiletime.*
import scala.reflect.ClassTag
import scala.deriving.Mirror

case class WithTags[+S](step: S, tag: Option[String], iTags: Map[String, String]):
  def mkTag             = StepUtils.resolve(tag)
  def mkITag(k: String) = StepUtils.resolve(iTags.get(k))
  def stepName          = step.getClass().getSimpleName().nn

/** Provides details for executing a pipeline step.
  *
  * Remarks for json en-/decoding:
  *   - the simple name of S must be unique
  *   - the encoding of S must not have fields that are also tags in ITags
  *   - the fields `type` and `tag` are reserved
  */
trait StepImpl[S: Encoder.AsObject: Decoder: ClassTag]:
  transparent inline def stagesUsed: Any
  transparent inline def stagesModified: Any

  def tags: List[String]

  def runToStage(s: WithTags[S], cache: StageCache): Either[String, RunningTime.Measured[?]]

  def helpText: String

  def stepName: String = summon[ClassTag[S]].runtimeClass.getSimpleName().nn

  def codec: Codec[WithTags[S]] = Codec.from(taggedDec, taggedEnc)

  def taggedEnc(using enc: Encoder.AsObject[S]): Encoder[WithTags[S]] =
    Encoder.AsObject.instance[WithTags[S]]: swt =>
      val more = List("type" -> swt.step.getClass.getSimpleName.nn.asJson, "tag" -> swt.tag.asJson)
        ++ swt.iTags.map((tag, value) => tag.toString -> value.asJson)
      more.foldLeft(enc.encodeObject(swt.step))(_.add.tupled(_))

  def taggedDec(using Decoder[S]): Decoder[WithTags[S]] =
    def decodeTags(json: JsonObject) =
      tags.traverse(tag => json(tag).traverse(_.as[String]).map(_.map(tag -> _))).map(_.flatten.toMap).toTry

    for
      s    <- Decoder[S]
      _    <- Decoder[String].at("type").emap: tpe =>
                if tpe == s.getClass.getSimpleName.nn then Right(())
                else Left(s"expected type to be ${s.getClass.getSimpleName}")
      tag  <- Decoder[Option[String]].at("tag")
      tags <- Decoder.decodeJsonObject.emapTry(decodeTags)
    yield WithTags(s, tag, tags)
  end taggedDec
end StepImpl

object StepImpl:
  type Aux[S, X <: Tuple] = StepImpl[S] { type ITags = X }

  def apply[T](using s: StepImpl[T]) = s

  transparent inline def allImpls[T](using m: Mirror.SumOf[T]): List[Any] =
    summonAll[Tuple.Map[m.MirroredElemTypes, [z] =>> StepImpl[z]]].toList

object StepUtils:
  def resolve(t: Option[String]) = t.getOrElse("default")

  extension [T, R](eth: Either[T, Unit])
    def unit: Either[T, RunningTime.Measured[Unit]] = eth.map(_ => RunningTime.unit)

  trait UseStages[T]:
    type R <: Tuple
    def apply(s: WithTags[?], cache: StageCache, t: T): Either[String, R]

  object UseStages:
    def apply[T](s: WithTags[?], cache: StageCache, t: T)(using use: UseStages[T]) = use(s, cache, t)

    given UseStages[EmptyTuple] with
      type R = EmptyTuple
      def apply(s: WithTags[?], cache: StageCache, t: EmptyTuple) = Right(EmptyTuple)

    given [R1, Rn <: Tuple, T <: Tuple](using use: UseStages[T] { type R = Rn }): UseStages[(String, Stage[R1]) *: T]
    with
      type R = R1 *: Rn
      def apply(s: WithTags[?], cache: StageCache, t: ((String, Stage[R1]) *: T)) = for
        head <- cache.getStageResult(t.head._2, s.mkITag(t.head._1))
        tail <- use(s, cache, t.tail)
      yield head *: tail
  end UseStages

  object UseSingleStage:
    def apply[T0, R0](s: WithTags[?], cache: StageCache, t: T0)(using
        use: UseStages[T0 *: EmptyTuple] { type R = R0 *: EmptyTuple },
    ): Either[String, R0] = use(s, cache, t *: EmptyTuple).map(_.head)

  trait GetTags[T]:
    def apply(t: T): List[String]

  object GetTags:
    def apply[T](t: T)(using get: GetTags[T]) = get(t)

    given GetTags[EmptyTuple] with
      def apply(t: EmptyTuple) = Nil

    given [T <: Tuple, R](using get: GetTags[T]): GetTags[(String, Stage[R]) *: T] with
      def apply(t: ((String, Stage[R]) *: T)) = t.head._1 :: get(t.tail)

  object GetSingleTag:
    def apply[T0](t: T0)(using get: GetTags[T0 *: EmptyTuple]) = get(t *: EmptyTuple)

  trait UpdateStages[T]:
    type In <: Tuple
    def apply(s: WithTags[?], cache: StageCache, t: T, in: In): Either[String, Unit]

  object UpdateStages:
    def apply[T](s: WithTags[?], cache: StageCache, t: T)(using update: UpdateStages[T])(in: update.In) =
      update(s, cache, t, in)

    given UpdateStages[EmptyTuple] with
      type In = EmptyTuple
      def apply(s: WithTags[?], cache: StageCache, t: EmptyTuple, in: EmptyTuple) = Right(())

    given [T <: Tuple, I, InN <: Tuple](using update: UpdateStages[T] { type In = InN }): UpdateStages[Stage[I] *: T]
    with
      type In = I *: InN
      def apply(s: WithTags[?], cache: StageCache, t: (Stage[I] *: T), in: In) = for
        _ <- cache.setStage(t.head, s.mkTag, in.head)
        _ <- update(s, cache, t.tail, in.tail)
      yield ()
  end UpdateStages

  object UpdateSingleStage:
    def apply[T0, I0](s: WithTags[?], cache: StageCache, t: T0)(using
        update: UpdateStages[T0 *: EmptyTuple] { type In = I0 *: EmptyTuple },
    )(in: I0) = update(s, cache, t *: EmptyTuple, in *: EmptyTuple)
end StepUtils
