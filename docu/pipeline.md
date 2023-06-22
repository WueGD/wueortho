WueOrtho Pipeline Format
========================

*This document was generated on 2023-06-22. Use the approriate main to generate an up-to-date version.*

A WueOrtho pipeline can be assembled using its JSON representation.
The pipeline describes a selection and oder of algorithms to produce an orthogonal graph drawing.
Each step may read and write intermediate results to a shared cache to persist results and to promote data to following steps.

Intermediate results are grouped by type into so-called stages.
Currently, there are the following stages:
 - Graph
 - Layout
 - VertexLabels
 - Obstacles
 - Ports
 - PortLabels
 - RoutingGraph
 - EdgeRouting
 - Routes
 - Svg
 - Metadata
 - ForeignData

In order to execute pipelines, we provide runtimes with different feature scopes.
Runtimes define which steps a pipeline can have and provide implementations for each step.
This text was compiled for the 'core-runtime' runtime. Different runtimes may diverge in naming and functionality.
A pipeline for this runtime may produce different results or may not even run with other runtimes.

A pipeline definition is a json object `{ "steps": [<step object>] }`.
Each step is defined by a step object `{ "type": "<step type>" }` with potentially more keys to configure the step's behavior.
For example the angle heuristic used to distribute ports on vertex boxes may be defined as follows.
```
{
  "mode" : "OnlyVertical",
  "type" : "PortsByAngle",
  "tag" : "vertical",
  "graph" : "sample"
}
```
Each step is identified by its `type` key. The PortsByAngle step additionally has a mandatory `mode` key.
The `tag` key is a special key for all Steps that defines an id that is attached to all stages written by this step.
Some steps have input tags (like `graph` above). These can be used to select stages from which this step obtains its inputs.

The following steps are available with this runtime.

**RandomGraph**

Create graphs at random.
 * The PRNG is generated using `seed`.
 * The graph will have `n` vertices and `m` edges.
 * `core` - allows to specify a graph structure for connectivity. Possible cores are `Empty`, `Path`, `Tree`, `Star`.
 * `allowLoops` - enable self-edges.

*Input Tags*: -


**RandomVertexBoxes**

Create vertex boxes at random.
 * `minSpan`/`maxSpan - minimum/maximum span vector of the boxes (span.x = width/2, span.y = height/2)
 * `seed` - the PRNG is created using this

*Input Tags*: `graph`


**UniformVertexBoxes**

Create vertex boxes of uniform size.
 * `span` - span vector of the boxes (span.x = width/2, span.y = height/2)

*Input Tags*: `vertexLayout`


**SyntheticVertexLabels**

Create artificial vertex labels.
 * `config` - is either `Hide` or `Enumerate`

*Input Tags*: `graph`


**SyntheticPortLabels**

Create artificial port labels.
 * `config` - is either `Hide` or `Enumerate`

*Input Tags*: `ports`


**BoxesFromLabels**

Create vertex boxes to host text labels
 * `config` - either `PralineDefaults` or a json object with:
   - `minWidth`/`minHeight` - minimum width and height.
   - `padding` - at all sides.
   - `fontSize`

*Input Tags*: `vertexLayout`, `vertexLabels`


**ReadTglfFile**

Read imputs in Trivial Graph Layout Format.
 * `path` - read from this file.
 * `use` - select a list of extractors. Possible values: `Graph`, `VertexLayout`, `Obstacles`, `EdgeRoutes`

*Input Tags*: -


**ForceDirectedLayout**

Perform force-directed vertex layout for a given graph.
 * `seed` - The layout is initialized using a PRNG with this seed.
 * `iterations` - and the algorithm stops after so many steps.
 * `repetitions` - number of layouts will be calculated. The algorithm takes the one with the least straight-line crossings

*Input Tags*: `graph`


**GTreeOverlaps**

Remove overlaps among vertex boxes with the GTree algorithm.
 * `stretch` - manipulate the boxes before removing overlaps.
 * `seed` - use a PRNG initialized with this seed.
 * `forceGeneralPosition` - manipulate vertex positions afterwards to ensure general position.

*Input Tags*: `vertexBoxes`


**PortsByAngle**

Distribute ports based on straight-line edges.
 * `mode` - use one of `OnlyVertical`, `OnlyHorizontal`, `Quadrants`, or `Octants`

*Input Tags*: `vertexBoxes`, `graph`


**SimplifiedRoutingGraph**

Create a routing graph.
 * `stretch` - manipulate the boxes before routing

*Input Tags*: `vertexBoxes`, `ports`


**EdgeRouting**

Perform edge routing (includes edge order).

*Input Tags*: `routingGraph`, `ports`


**PseudoRouting**

Produce a fake edge routing from already routed edges
(e.g. in order to apply a nudging step afterwards).

*Input Tags*: `routes`


**NoNudging**

Perform no nudging.

*Input Tags*: `routing`


**ConstrainedNudging**

Perform constrained nudging.

*Input Tags*: `routing`, `ports`, `vertexBoxes`


**FullNudging**

Perform full nudging (moves edge segments, ports, and vertex boxes).
 * `padding` - A minimum object distance is maintained.
 * `use2ndHPass` - enables an additional horizontal pass of full nudging.

*Input Tags*: `routing`, `vertexBoxes`, `ports`, `graph`


**Metrics**

Calculate metrics.
 * `use` - select a list of metrics. Use `["all"]` to select all metrics.

*Input Tags*: `routes`, `graph`, `vertexBoxes`


**SvgDrawing**

Draw as SVG.
 * `config` - use a predefined config:

   - `SmoothEdges` colorful smooth edges (ppu=50).
   - `StraightEdges` colorful straight edges (ppu=50).
   - `Praline` close to Praline but with colorful edges (ppu=1).
   - `Custom` full custom (see wueortho.io.svg.Svg for details).

 * `overridePpu` - override the pixels per unit setting [optional]

*Input Tags*: `routes`, `vertexBoxes`, `vertexLabels`, `portLabels`


**SvgToFile**

Save the SVG as `path`

*Input Tags*: `svg`


**ReadPralineFile**

Read praline json from file.
 * `path`
 * `use` - configure what data to load. Options are `Graph`, `VertexLabels`, `VertexLayout`, `VertexBoxes`, `EdgeRoutes`.

*Input Tags*: -


**AccessPraline**

Access the praline API via the ForeignData stage.
 * `use` - configure what data to load. Options are `Graph`, `VertexLabels`, `VertexLayout`, `VertexBoxes`, `EdgeRoutes`.

*Input Tags*: `praline`


**WritePralineFile**

Store pipeline contents to file as praline json.
All availabe stages will be included. Use undefined tags to exclude stages.

*Input Tags*: `graph`, `vertexBoxes`, `vertexLabels`, `routes`


**StorePraline**

Store pipeline contents to the praline API via the ForeignData stage.
All availabe stages will be included. Use undefined tags to exclude stages.

*Input Tags*: `praline`, `graph`, `vertexBoxes`, `vertexLabels`, `routes`

