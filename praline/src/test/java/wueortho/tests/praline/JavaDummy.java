package wueortho.tests.praline;

import de.uniwue.informatik.praline.datastructure.graphs.Graph;
import wueortho.interop.JavaApi;
import wueortho.pipeline.PipelineResult;

class JavaDummy {
	private final JavaApi api;

	public JavaDummy(Graph g) {
		api = new JavaApi();
		var ref = api.getGraphBox();
		ref.set(g);
	}

	public PipelineResult run() {
		api.setMainTag("praline");
		var pipeline = api.loadPipeline(raw);
		var result = api.run(pipeline);
		// System.out.println(result.runningTime().show());
		return result;
	}

	static String raw = """
			{ "steps": [
				{
					"type": "AccessPraline",
					"use": ["Graph", "VertexLayout", "VertexLabels"],
					"praline": "praline"
				}, {
					"type": "BoxesFromLabels",
					"config": { "type": "PralineDefaults" }
				}, {
					"type": "PortsByAngle",
					"mode": "Octants"
				}, {
					"type": "SimplifiedRoutingGraph",
					"stretch": { "type": "Original" }
				}, {
					"type": "EdgeRouting",
					"seed": "c1a6f00d",
					"useCenteredRouting": false
				}, {
					"type": "FullNudging",
					"padding": 12,
					"use2ndHPass": true
				}, {
					"type": "SyntheticPortLabels",
					"config": "Hide"
				}, {
					"type": "SvgDrawing",
					"config": { "type": "Praline" }
				}, {
					"type": "SvgToFile",
					"path": "test-results/java-dummy.svg"
				}, {
					"type": "StorePraline",
					"praline": "praline"
				}
			] }""";
}
