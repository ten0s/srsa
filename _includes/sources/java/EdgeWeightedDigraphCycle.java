public class EdgeWeightedDigraphCycle {
    //+BEGIN_SOLUTION
    private boolean[] marked;
    private DirectedEdge[] edgeTo;
    private boolean[] onStack;
    private Stack<DirectedEdge> cycle;
    //+END_SOLUTION

    public EdgeWeightedDigraphCycle(EdgeWeightedDigraph G) {
        //+BEGIN_SOLUTION
        marked = new boolean[G.V()];
        edgeTo = new DirectedEdge[G.V()];
        onStack = new boolean[G.V()];
        for (int v = 0; v < G.V(); v++) {
            if (!marked[v]) {
                dfs(G, v);
            }
        }
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private void dfs(EdgeWeightedDigraph G, int v) {
        marked[v] = true;
        onStack[v] = true;
        for (DirectedEdge e : G.adj(v)) {
            // short circuit if cycle already found
            if (hasCycle()) return;
            int w = e.to();
            if (!marked[w]) {
                edgeTo[w] = e;
                dfs(G, w);
            } else if (onStack[w]) {
                cycle = new Stack<>();
                DirectedEdge f = e;
                while (f.from() != w) {
                    f = edgeTo[f.from()];
                    cycle.push(f);
                }
                cycle.push(e);
            }
        }
        onStack[v] = false;
    }
    //+END_SOLUTION

    public boolean hasCycle() {
        //+BEGIN_SOLUTION
        return cycle != null;
        //+END_SOLUTION
    }

    public Iterable<DirectedEdge> cycle() {
        //+BEGIN_SOLUTION
        return cycle;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // /data/ewdigraph3.txt
        // no cycles
        EdgeWeightedDigraph G3 = new EdgeWeightedDigraph(6);
        G3.addEdge(new DirectedEdge(0, 2, 0.35));
        G3.addEdge(new DirectedEdge(1, 2, 0.37));
        G3.addEdge(new DirectedEdge(2, 3, 0.28));
        G3.addEdge(new DirectedEdge(3, 4, 0.17));
        G3.addEdge(new DirectedEdge(3, 5, 0.19));
        EdgeWeightedDigraphCycle c3 = new EdgeWeightedDigraphCycle(G3);
        Assert.assertFalse(c3.hasCycle());

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
        EdgeWeightedDigraphCycle c4 = new EdgeWeightedDigraphCycle(G4);
        Assert.assertFalse(c4.hasCycle());

        // /data/ewdigraph4-cycle.txt
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
        EdgeWeightedDigraphCycle c4c = new EdgeWeightedDigraphCycle(G4C);
        Assert.assertTrue(c4c.hasCycle());
        Assert.assertEquals("6->0->2->4->6", GraphUtil.directedWeightedPathToString(c4c.cycle));

        System.out.println("OK");
    }
    //+END_FOLD }
}

// Refs
/*+BEGIN_FOLD
public class public class Stack<Item> implements Iterable<Item> {
    public Stack();
    public void push(Item item);
}
+END_FOLD*/
