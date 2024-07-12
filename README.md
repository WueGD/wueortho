WueOrtho
========

Orthogonal multigraph drawings optimized for compactness.

WueOrtho provides a simple but complete pipeline for producing compact orthogonal
drawings of connected multigraphs.  When used standalone, it utilizes force-directed
drawing for vertex positioning and orthogonal routing with port generation for edges.
The WueOrtho algorithm pipeline is highly flexible and can be used as a post-processing
step to other drawing algorithms in order to achieve higher compactness (i.e. fit the
drawing into a smaller area).


Project Structure
-----------------

WueOrtho is implemented in Scala 3.  The project is split into the core, layout, io,
pipeline, and praline subprojects.

#### Core

Core contains basic data structures and utilities common to all subprojects.
It has no further dependencies.

#### Layout

Layout contains all layouting algorithms.  If you only require basic layouting
support and provide your own data conversion and rendering routines, it is sufficient
to only depend on layout (which itself only depends on core).

#### IO

IO contains input format handling and rendering support.  Currently, WueOrtho
supports the TG(L)F input format and provides rendering into the SVG format.

#### Pipeline

WueOrtho is a simple pipeline of mostly independent algorithms.  The Pipeline
subproject holds means to choose and configure the set of algorithms that fits
your requirements best.

#### Praline

Praline provides interoperability with the Praline project.  This currently
contains reading and writing the Praline file format and utilizing partial
results from the praline layouting.


Quick Start Guide
-----------------

This project requires at least JDK17.

*Setup Praline:* in order to use the Praline interoperability module (and
compile the root module), the Praline binaries must be provided to sbt as
unmanaged jars.  Build the `Praline-IO`, `Praline-Data-Structure`, and
`Praline-Layouting` libraries and place the resulting jar files in
`./praline/lib`.

*Setup your Pipeline:* copy the `config.json` file from `./docu` into the
project directory or create a new pipeline configuration.

*Run the Pipeline:* the root project has a main method to run your pipeline.
Start sbt or use the provided wrapper script `./sbtx` and execute the `run`
task.  Select `runPipeline` when prompted or select `runInteropPipeline` if
your want to use pipeline steps that utilize Praline.

*Pipeline Configuration:* find detailed instructions at `./docu/pipeline.md`.


Publication
-----------

Most of the algorithms are described in

Tim Hegemann, Alexander Wolff. [A Simple Pipeline for Orthogonal Graph Drawing.](https://doi.org/10.1007/978-3-031-49275-4_12)
GD2023, vol. 14466 of LNCS, pp. 170–186, Springer. ([arxiv](https://arxiv.org/abs/2309.01671))


Contributors
------------

This library has been developed by the [algorithms and complexity group](https://www.informatik.uni-wuerzburg.de/algo/team/),
University of Würzburg, Germany.

For contact, you can write an email to ``praline *at* informatik *dot* uni-wuerzburg *dot* de``.


Funding
-------

This work was supported by BMBF grant 01IS22012C.