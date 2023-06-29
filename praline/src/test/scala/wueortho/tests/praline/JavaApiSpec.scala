package wueortho.tests.praline

import wueortho.data.PortLayout
import wueortho.pipeline.{Stage, Debugging}
import wueortho.interop.PralineReader, PralineReader.syntax.*

import de.uniwue.informatik.praline.datastructure.graphs.Graph as PGraph

import java.nio.file

import scala.util.Using
import org.scalatest.flatspec.AnyFlatSpec

class JavaApiSpec extends AnyFlatSpec:
  lazy val input = Using.resource(getClass.getResourceAsStream("/sample.json").nn): stream =>
    PralineReader.fromInputStream(stream).get

  "The java API" `should` "be able to run a pipeline" in:
    val uut   = new JavaDummy(input)
    val res   = uut.run().nn
    val ref   = res.getResult(Stage.ForeignData, Some("praline")).toOption.get
    val graph = ref.get.asInstanceOf[PGraph]
    (for
      vb <- graph.getVertexBoxes
      er <- graph.getEdgeRoutes
    yield
      val svg = Debugging.debugSvg(vb, PortLayout(er.map(_.terminals)), er, 1.0)
      file.Files.writeString(file.Path.of("test-results", "java-dummy2.svg"), svg)
    ).fold(sys.error, identity)
end JavaApiSpec
