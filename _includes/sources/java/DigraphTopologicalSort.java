public class DigraphTopologicalSort {
    //+BEGIN_SOLUTION
    private Iterable<Integer> order;
    //+END_SOLUTION

    public DigraphTopologicalSort(Digraph G) {
        //+BEGIN_SOLUTION
        DigraphCycle cycleFinder = new DigraphCycle(G);
        if (!cycleFinder.hasCycle()) {
            DigraphOrders orders = new DigraphOrders(G);
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
        //System.out.println(G4);
        //System.out.println(G4.toDot());

        DigraphTopologicalSort ts4 = new DigraphTopologicalSort(G4);
        Assert.assertTrue(ts4.hasOrder());
        Assert.assertEquals("0-2-4-6-5-1-3", GraphUtil.pathToString(ts4.order()));

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
        //System.out.println(G4C);
        //System.out.println(G4C.toDot());

        DigraphTopologicalSort ts4c = new DigraphTopologicalSort(G4C);
        Assert.assertFalse(ts4c.hasOrder());
        Assert.assertNull(ts4c.order());

        System.out.println("OK");
    }
    //+END_FOLD }
}

// Refs
/*+BEGIN_FOLD
public class DigraphCycle {
    public DigraphCycle(Digraph G);
    public boolean hasCycle();
}

public class DigraphOrders {
    public DigraphOrders(Digraph G);
    public Iterable<Integer> preOrder();
    public Iterable<Integer> postOrder();
    public Iterable<Integer> reversedPostOrder();
}
+END_FOLD*/
