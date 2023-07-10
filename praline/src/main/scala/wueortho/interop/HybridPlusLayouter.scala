package wueortho.interop

import de.uniwue.informatik.praline.layouting.PralineLayouter
import de.uniwue.informatik.praline.datastructure.graphs.Graph
import wueortho.interop.{PralinePipelineExtensions as PPE}
import wueortho.pipeline.*, PipelineStep.just
import PPE.PralineExtractor as Use

abstract class HybridPlusLayouter(
    graph: Graph,
    minObjDistance: Double,
    minVertexBoxWidth: Double,
    minVertexBoxHeight: Double,
    vertexLabelPadding: Double,
    labelFontSize: Int,
) extends PralineLayouter:
  protected val runtime  = PPE.InteropRuntime(CoreStep.allImpls ++ PPE.allImpls)
  protected val pipeline =
    val labelConfig = VertexLabelConfig.Custom(minVertexBoxWidth, minVertexBoxHeight, vertexLabelPadding, labelFontSize)
    Pipeline:
        Seq(
          just(PPE.AccessPraline(List(Use.Graph, Use.VertexLayout, Use.VertexLabels, Use.EdgeRoutes))),
          just(step.BoxesFromLabels(labelConfig)),
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
