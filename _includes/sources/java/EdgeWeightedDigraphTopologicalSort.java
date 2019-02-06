public class EdgeWeightedDigraphTopologicalSort {
    // SOLUTION_BEGIN
    private Iterable<Integer> order;
    // SOLUTION_END

    public EdgeWeightedDigraphTopologicalSort(EdgeWeightedDigraph G) {
        // SOLUTION_BEGIN
        EdgeWeightedDigraphCycle cycleFinder = new EdgeWeightedDigraphCycle(G);
        if (!cycleFinder.hasCycle()) {
            EdgeWeightedDigraphOrders orders = new EdgeWeightedDigraphOrders(G);
            order = orders.reversedPostOrder();
        }
        // SOLUTION_END
    }

    public boolean hasOrder() {
        // SOLUTION_BEGIN
        return order != null;
        // SOLUTION_END
    }

    public Iterable<Integer> order() {
        // SOLUTION_BEGIN
        return order;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        // /data/ewdigraph4.txt
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
        // NB:
        // EdgeWeightedDigraph.addEdge(v, w) prepends vertices,
        // so EdgeWeightedDigraph.adj(v) returns them in reverse order
        Assert.assertTrue(ts4.hasOrder());
        Assert.assertEquals("0-1-2-3-4-5-6", GraphUtil.pathToString(ts4.order()));

        // /data/digraph4-cycle.txt
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
}

/*
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
*/
