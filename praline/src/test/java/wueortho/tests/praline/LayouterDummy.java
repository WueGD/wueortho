package wueortho.tests.praline;

import de.uniwue.informatik.praline.io.output.util.DrawingInformation;
import de.uniwue.informatik.praline.datastructure.graphs.Graph;

class LayouterDummy extends wueortho.interop.HybridPlusLayouter {
  private DrawingInformation infos;

  public LayouterDummy(Graph g, double minObjDistance) {
    super(g, minObjDistance);
  }

  @Override public void setDrawingInformation(DrawingInformation di) {
    this.infos = di;
  }

  @Override public DrawingInformation getDrawingInformation() {
    return this.infos;
  }

  public static Graph run(Graph g, double minObjDistance) {
    var dummy = new LayouterDummy(g, minObjDistance);
    dummy.computeLayout();
    return dummy.getGraph();
  }
}
