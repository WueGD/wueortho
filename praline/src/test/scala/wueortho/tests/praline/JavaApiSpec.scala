package wueortho.tests.praline

import wueortho.data.PortLayout
import wueortho.pipeline.*
import wueortho.interop.{PralinePipelineExtensions as PPE, *}, PralineReader.syntax.*

import de.uniwue.informatik.praline.datastructure.graphs.Graph as PGraph
import de.uniwue.informatik.praline.io.output.util.DrawingInformation
import de.uniwue.informatik.praline.io.output.svg.SVGDrawer
import java.nio.file

import scala.util.Using
import org.scalatest.flatspec.AnyFlatSpec

class JavaApiSpec extends AnyFlatSpec:
  def input = Using.resource(getClass.getResourceAsStream("/sample.json").nn): stream =>
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

  "The force-directed PralineLayouter implementation" `should` "run without errors" in:
    val layouter = new ForceDirectedLayouter(input, 12, 16, 34, 2, 12, true):
      override def getDrawingInformation()                              = sys.error("use of stupid api")
      override def setDrawingInformation(di: DrawingInformation | Null) = sys.error("use of stupid api")

    layouter.computeLayout()
    rt.ref.set(layouter.getGraph().nn)
    rt.run(DebuggingStep.justDraw("force-directed_java-api.svg"))

  "The rerouting PralineLayouter implementation" `should` "run without errors" in:
    val layouter = new ReroutingLayouter(input, 12, 16, 34, 2, 12, false):
      override def getDrawingInformation()                              = sys.error("use of stupid api")
      override def setDrawingInformation(di: DrawingInformation | Null) = sys.error("use of stupid api")

    layouter.computeLayout()
    rt.ref.set(layouter.getGraph().nn)
    rt.run(DebuggingStep.justDraw("rerouting_java-api.svg"))

  "A Java hybrid+ PralineLayouter implementation" `should` "run when implemented as java class" in:
    rt.ref.set(LayouterDummy.run(input, 12).nn)
    rt.run(DebuggingStep.justDraw("hybrid-plus-java.svg"))

  it `should` "allow drawing with the Praline drawer" in:
    SVGDrawer(LayouterDummy.run(input, 12))
      .draw("test-results/hybrid-plus-java_praline-drawer.svg", DrawingInformation())

  def hybridPlusLayouter = new HybridPlusLayouter(input, 12):
    override def getDrawingInformation()                              = sys.error("use of stupid api")
    override def setDrawingInformation(di: DrawingInformation | Null) = sys.error("use of stupid api")

  "A Scala hybrid+ PralineLayouter implementation" `should` "allow drawing with the Praline drawer" in:
    val layouter = hybridPlusLayouter
    layouter.computeLayout()
    SVGDrawer(layouter.getGraph()).draw("test-results/hybrid-plus-scala_praline-drawer.svg", DrawingInformation())

  it `should` "allow drawing with the WueOrtho drawer" in:
    val layouter = hybridPlusLayouter
    layouter.computeLayout()
    rt.ref.set(layouter.getGraph().nn)
    rt.run(DebuggingStep.justDraw("hybrid-plus-scala.svg"))
end JavaApiSpec
