// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.interop

import wueortho.pipeline.*, PipelineStep.{just, withTags}
import wueortho.data.Seed

import de.uniwue.informatik.praline.layouting.PralineLayouter
import de.uniwue.informatik.praline.datastructure.graphs.Graph

import PralinePipelineExtensions as PPE, PPE.PralineExtractor as Use

abstract class ForceDirectedLayouter(
    graph: Graph,
    minObjDistance: Double,
    minVertexBoxWidth: Double,
    minVertexBoxHeight: Double,
    vertexLabelPadding: Double,
    labelFontSize: Int,
    useHorizontalPorts: Boolean,
) extends PralineLayouter:
  protected val runtime  = PPE.InteropRuntime(CoreStep.allImpls ++ PPE.allImpls)
  protected val pipeline =
    val labelConfig = VertexLabelConfig.Custom(minVertexBoxWidth, minVertexBoxHeight, vertexLabelPadding, labelFontSize)
    Pipeline:
        Seq(
          just(PPE.AccessPraline(List(Use.Graph, Use.VertexLabels))),
          just(step.ForceDirectedLayout(iterations = 800, seed = Seed(0x99c0ffee), repetitions = 1)),
          just(step.BoxesFromLabels(labelConfig)),
          just(step.GTreeOverlaps(Stretch.Uniform(1.4), Seed(0x99c0ffee), forceGeneralPosition = true)),
          just(step.PortsByAngle(if useHorizontalPorts then PortMode.Octants else PortMode.OnlyVertical)),
          just(step.SimplifiedRoutingGraph(Stretch.Original)),
          just(step.EdgeRouting(seed = Seed(0x98c0ffee), useCenteredRouting = false)),
          just(step.FullNudging(minObjDistance, use2ndHPass = true)),
          withTags(PPE.UpdatePraline(), None)("vertexLabels" -> "disabled"),
        )
  end pipeline

  override def computeLayout() =
    runtime.ref.set(graph)
    runtime.run(pipeline)
    ()

  override def getGraph() = Option(runtime.ref.get()).getOrElse(graph)
end ForceDirectedLayouter
