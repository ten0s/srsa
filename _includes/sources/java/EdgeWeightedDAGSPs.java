public class EdgeWeightedDAGSPs {
    private static final double INFINITY = Double.POSITIVE_INFINITY;
    //+BEGIN_SOLUTION
    private DirectedEdge[] edgeTo;
    private double[] distTo;
    //+END_SOLUTION

    public EdgeWeightedDAGSPs(EdgeWeightedDigraph G, int s) {
        //+BEGIN_SOLUTION
        edgeTo = new DirectedEdge[G.V()];
        distTo = new double[G.V()];

        for (int v = 0; v < G.V(); v++) {
            distTo[v] = INFINITY;
        }
        distTo[s] = 0.0;

        EdgeWeightedDigraphTopologicalSort top = new EdgeWeightedDigraphTopologicalSort(G);
        if (top.hasOrder()) {
            for (int v : top.order()) {
                relax(G, v);
            }
        } else {
            throw new IllegalArgumentException("Given graph is NOT a DAG!");
        }
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private void relax(EdgeWeightedDigraph G, int v) {
        for (DirectedEdge e : G.adj(v)) {
            int w = e.to();
            if (distTo[w] > distTo[v] + e.weight()) {
                distTo[w] = distTo[v] + e.weight();
                edgeTo[w] = e;
            }
        }
    }
    //+END_SOLUTION

    public double distTo(int v) {
        //+BEGIN_SOLUTION
        return distTo[v];
        //+END_SOLUTION
    }

    public boolean hasPathTo(int v) {
        //+BEGIN_SOLUTION
        return distTo[v] < INFINITY;
        //+END_SOLUTION
    }

    public Iterable<DirectedEdge> pathTo(int v) {
        //+BEGIN_SOLUTION
        if (!hasPathTo(v)) return null;
        Stack<DirectedEdge> path = new Stack<>();
        for (DirectedEdge e = edgeTo[v]; e != null; e = edgeTo[e.from()]) {
            path.push(e);
        }
        return path;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // /data/tinyEWDAG.txt
        EdgeWeightedDigraph G = new EdgeWeightedDigraph(8);
        G.addEdge(new DirectedEdge(5, 4, 0.35));
        G.addEdge(new DirectedEdge(4, 7, 0.37));
        G.addEdge(new DirectedEdge(5, 7, 0.28));
        G.addEdge(new DirectedEdge(5, 1, 0.32));
        G.addEdge(new DirectedEdge(4, 0, 0.38));
        G.addEdge(new DirectedEdge(0, 2, 0.26));
        G.addEdge(new DirectedEdge(3, 7, 0.39));
        G.addEdge(new DirectedEdge(1, 3, 0.29));
        G.addEdge(new DirectedEdge(7, 2, 0.34));
        G.addEdge(new DirectedEdge(6, 2, 0.40));
        G.addEdge(new DirectedEdge(3, 6, 0.52));
        G.addEdge(new DirectedEdge(6, 0, 0.58));
        G.addEdge(new DirectedEdge(6, 4, 0.93));
        //System.out.println(G);
        //System.out.println(G.toDot());

        EdgeWeightedDAGSPs sp0 = new EdgeWeightedDAGSPs(G, 0);
        Assert.assertTrue(sp0.hasPathTo(0));
        Assert.assertEquals(0.0, sp0.distTo(0));
        Assert.assertEquals("", GraphUtil.directedWeightedPathToString(sp0.pathTo(0)));

        Assert.assertFalse(sp0.hasPathTo(5));
        Assert.assertEquals(INFINITY, sp0.distTo(5));
        Assert.assertNull(sp0.pathTo(5));

        EdgeWeightedDAGSPs sp5 = new EdgeWeightedDAGSPs(G, 5);
        Assert.assertTrue(sp5.hasPathTo(0));
        Assert.assertEquals(0.73, sp5.distTo(0));
        Assert.assertEquals("5->4->0", GraphUtil.directedWeightedPathToString(sp5.pathTo(0)));

        Assert.assertTrue(sp5.hasPathTo(6));
        Assert.assertEquals(1.13, sp5.distTo(6));
        Assert.assertEquals("5->1->3->6", GraphUtil.directedWeightedPathToString(sp5.pathTo(6)));

        G.addEdge(new DirectedEdge(2, 5, 0.0));
        try {
            new EdgeWeightedDAGSPs(G, 0);
            Assert.assertTrue("No check if the graph is a DAG", false);
        } catch (IllegalArgumentException e) {}

        System.out.println("OK");
    }
    //+END_FOLD }
}

// Refs
/*+BEGIN_FOLD
public class DirectedEdge implements Comparable<DirectedEdge> {
    public DirectedEdge(int v, int w, double weight);
    public double weight();
    public int from();
    public int to();
}

public class EdgeWeightedDigraphTopologicalSort {
    public EdgeWeightedDigraphTopologicalSort(EdgeWeightedDigraph G);
    public boolean hasOrder();
    public Iterable<Integer> order();
}

public class public class Stack<Item> implements Iterable<Item> {
    public Stack();
    public void push(Item item);
}
+END_FOLD*/
