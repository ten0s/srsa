/*
public class DigraphCycle {
    public DigraphCycle(Digraph G);
    public boolean hasCycle();
}

public class DigraphOrders {
    public DigraphOrders(Digraph G);
    public Iterable<Integer> reversedPostOrder();
}
*/

public class DigraphTopologicalSort {
    // SOLUTION_BEGIN
    private Iterable<Integer> order;
    // SOLUTION_END

    public DigraphTopologicalSort(Digraph G) {
        // SOLUTION_BEGIN
        DigraphCycle cycleFinder = new DigraphCycle(G);
        if (!cycleFinder.hasCycle()) {
            DigraphOrders orders = new DigraphOrders(G);
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
        // /data/digraph4.txt
        // no cycles
        Digraph G4 = new Digraph(7);
        G4.addEdge(0, 1);
        G4.addEdge(0, 2);
        G4.addEdge(0, 5);
        G4.addEdge(0, 6);
        G4.addEdge(1, 3);
        G4.addEdge(2, 3);
        G4.addEdge(2, 4);
        G4.addEdge(4, 5);
        G4.addEdge(4, 6);
        DigraphTopologicalSort ts4 = new DigraphTopologicalSort(G4);
        // NB:
        // Digraph.addEdge(v, w) prepends vertices,
        // so Digraph.adj(v) returns them in reverse order
        Assert.assertTrue(ts4.hasOrder());
        Assert.assertEquals("0-1-2-3-4-5-6", GraphUtil.pathToString(ts4.order()));

        // /data/digraph4-cycle.txt
        // one cycle
        Digraph G4C = new Digraph(7);
        G4C.addEdge(0, 1);
        G4C.addEdge(0, 2);
        G4C.addEdge(0, 5);
        G4C.addEdge(1, 3);
        G4C.addEdge(2, 3);
        G4C.addEdge(2, 4);
        G4C.addEdge(4, 5);
        G4C.addEdge(4, 6);
        G4C.addEdge(6, 0);
        DigraphTopologicalSort ts4c = new DigraphTopologicalSort(G4C);
        Assert.assertFalse(ts4c.hasOrder());
        Assert.assertNull(ts4c.order());

        System.out.println("OK");
    }
}
