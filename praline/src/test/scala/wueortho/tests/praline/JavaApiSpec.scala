package wueortho.tests.praline

import wueortho.data.PortLayout
import wueortho.pipeline.*, PipelineStep.just
import wueortho.interop.{PralineReader, ForceDirectedLayouter, PralinePipelineExtensions as PPE},
  PralineReader.syntax.*, PPE.PralineExtractor as Use

import de.uniwue.informatik.praline.datastructure.graphs.Graph as PGraph
import de.uniwue.informatik.praline.io.output.util.DrawingInformation

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

  lazy val rt = PPE.InteropRuntime(CoreStep.allImpls ++ PPE.allImpls :+ DebuggingStep.impl)

  def justDraw(fileName: String) = Pipeline:
      Seq(
        just(PPE.AccessPraline(List(Use.Graph, Use.VertexBoxes, Use.VertexLabels, Use.EdgeRoutes))),
        just(step.SyntheticPortLabels(SyntheticLabels.Hide)),
        just(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
        just(step.SvgToFile(file.Path.of("test-results", fileName).nn)),
      )

  "The force-directed PralineLayouter implementation" `should` "run without errors" in:
    val layouter = new ForceDirectedLayouter(input, 12, 16, 34, 2, 12):
      override def getDrawingInformation()                              = sys.error("use of stupid api")
      override def setDrawingInformation(di: DrawingInformation | Null) = sys.error("use of stupid api")

    layouter.computeLayout()
    rt.ref.set(layouter.getGraph().nn)
    rt.run(justDraw("force-directed_java-api.svg"))

  "The hybrid+ PralineLayouter implementation" `should` "run when implemented as java class" in:
    rt.ref.set(LayouterDummy.run(input, 12).nn)
    rt.run(justDraw("hybrid-plus_plain-java.svg"))
end JavaApiSpec
