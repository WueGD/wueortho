package wueortho.interop

import de.uniwue.informatik.praline.layouting.PralineLayouter
import de.uniwue.informatik.praline.datastructure.graphs.Graph
import wueortho.interop.{PralinePipelineExtensions as PPE}
import wueortho.pipeline.*, PipelineStep.just
import PPE.PralineExtractor as Use

abstract class HybridPlusLayouter(graph: Graph, minObjDistance: Double) extends PralineLayouter:
  protected val runtime  = PPE.InteropRuntime(CoreStep.allImpls ++ PPE.allImpls)
  protected val pipeline =
    Pipeline:
        Seq(
          just(PPE.AccessPraline(List(Use.Graph, Use.VertexBoxes, Use.VertexLabels, Use.EdgeRoutes))),
          just(step.PseudoRouting(fakePorts = true)),
          just(step.FullNudging(minObjDistance, use2ndHPass = true)),
          just(PPE.StorePraline()),
        )
  end pipeline

  override def computeLayout() =
    runtime.ref.set(graph)
    runtime.run(pipeline)
    ()

  override def getGraph() = Option(runtime.ref.get()).getOrElse(graph)
end HybridPlusLayouter
