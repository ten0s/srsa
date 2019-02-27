public class EdgeWeightedDigraphTopologicalSort {
    //+BEGIN_SOLUTION
    private Iterable<Integer> order;
    //+END_SOLUTION

    public EdgeWeightedDigraphTopologicalSort(EdgeWeightedDigraph G) {
        //+BEGIN_SOLUTION
        EdgeWeightedDigraphCycle cycleFinder = new EdgeWeightedDigraphCycle(G);
        if (!cycleFinder.hasCycle()) {
            EdgeWeightedDigraphOrders orders = new EdgeWeightedDigraphOrders(G);
            order = orders.reversedPostOrder();
        }
        //+END_SOLUTION
    }

    public boolean hasOrder() {
        //+BEGIN_SOLUTION
        return order != null;
        //+END_SOLUTION
    }

    public Iterable<Integer> order() {
        //+BEGIN_SOLUTION
        return order;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/ewdigraph4.txt
        // no cycles
        EdgeWeightedDigraph G4 = new EdgeWeightedDigraph(7);
        G4.addEdge(new DirectedEdge(0, 1, 0.35));
        G4.addEdge(new DirectedEdge(0, 2, 0.37));
        G4.addEdge(new DirectedEdge(0, 5, 0.28));
        G4.addEdge(new DirectedEdge(0, 6, 0.16));
        G4.addEdge(new DirectedEdge(1, 3, 0.32));
        G4.addEdge(new DirectedEdge(2, 3, 0.38));
        G4.addEdge(new DirectedEdge(2, 4, 0.17));
        G4.addEdge(new DirectedEdge(4, 5, 0.19));
        G4.addEdge(new DirectedEdge(4, 6, 0.26));
        EdgeWeightedDigraphTopologicalSort ts4 = new EdgeWeightedDigraphTopologicalSort(G4);
        Assert.assertTrue(ts4.hasOrder());
        Assert.assertEquals("0-2-4-6-5-1-3", GraphUtil.pathToString(ts4.order()));

        // data/digraph4-cycle.txt
        // one cycle
        EdgeWeightedDigraph G4C = new EdgeWeightedDigraph(7);
        G4C.addEdge(new DirectedEdge(0, 1, 0.35));
        G4C.addEdge(new DirectedEdge(0, 2, 0.37));
        G4C.addEdge(new DirectedEdge(0, 5, 0.28));
        G4C.addEdge(new DirectedEdge(1, 3, 0.32));
        G4C.addEdge(new DirectedEdge(2, 3, 0.38));
        G4C.addEdge(new DirectedEdge(2, 4, 0.17));
        G4C.addEdge(new DirectedEdge(4, 5, 0.19));
        G4C.addEdge(new DirectedEdge(4, 6, 0.26));
        G4C.addEdge(new DirectedEdge(6, 0, 0.16));
        EdgeWeightedDigraphTopologicalSort ts4c = new EdgeWeightedDigraphTopologicalSort(G4C);
        Assert.assertFalse(ts4c.hasOrder());
        Assert.assertNull(ts4c.order());

        System.out.println("OK");
    }
    //+END_FOLD }
}

// Refs
/*+BEGIN_FOLD
public class EdgeWeightedDigraphCycle {
    public EdgeWeightedDigraphCycle(EdgeWeightedDigraph G);
    public boolean hasCycle();
}

public class EdgeWeightedDigraphOrders {
    public EdgeWeightedDigraphOrders(EdgeWeightedDigraph G);
    public Iterable<Integer> preOrder();
    public Iterable<Integer> postOrder();
    public Iterable<Integer> reversedPostOrder();
}
+END_FOLD*/
