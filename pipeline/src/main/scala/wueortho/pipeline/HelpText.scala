package wueortho.pipeline

import wueortho.util.EnumUtils.enumNames

object HelpText:
  private lazy val stages = enumNames[Stage[?]].map(s => s" - $s").mkString("\n")

  private lazy val exampleStep =
    import AlgorithmicSteps.given
    val pba = PipelineStep.withTags(step.PortsByAngle(PortMode.OnlyVertical), Some("vertical"))("graph" -> "sample")
    AlgorithmicSteps.given_StepImpl_PortsByAngle.codec(pba).spaces2

  private def mkStep(impl: StepImpl[?]) =
    def mkTags = if impl.tags.isEmpty then "-" else impl.tags.map(s => s"`$s`").mkString(", ")
    s"""**${impl.stepName}**
       |
       |${impl.helpText}
       |
       |*Input Tags*: $mkTags
       |
       |""".stripMargin
  end mkStep

  def apply(id: String, impls: Seq[StepImpl[?]]) =
    s"""WueOrtho Pipeline Format
       |========================
       |
       |A WueOrtho pipeline can be assembled using its JSON representation.
       |The pipeline describes a selection and oder of algorithms to produce an orthogonal graph drawing.
       |Each step may read and write intermediate results to a shared cache to persist results and to promote data to following steps.
       |
       |Intermediate results are grouped by type into so-called stages.
       |Currently, there are the following stages:
       |$stages
       |
       |In order to execute pipelines, we provide runtimes with different feature scopes.
       |Runtimes define which steps a pipeline can have and provide implementations for each step.
       |This text was compiled for the '$id' runtime. Different runtimes may diverge in naming and functionality.
       |A pipeline for this runtime may produce different results or may not even run with other runtimes.
       |
       |A pipeline definition is a json object `{ "steps": [<step object>] }`.
       |Each step is defined by a step object `{ "type": "<step type>" }` with potentially more keys to configure the step's behavior.
       |For example the angle heuristic used to distribute ports on vertex boxes may be defined as follows.
       |```
       |$exampleStep
       |```
       |Each step is identified by its `type` key. The PortsByAngle step additionally has a mandatory `mode` key.
       |The `tag` key is a special key for all Steps that defines an id that is attached to all stages written by this step.
       |Some steps have input tags (like `graph` above). These can be used to select stages from which this step obtains its inputs.
       |
       |The following steps are available with this runtime.
       |
       |${impls.map(mkStep).mkString("\n")}""".stripMargin
  end apply
end HelpText
