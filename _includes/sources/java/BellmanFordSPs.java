//+BEGIN_SOLUTION
import java.util.Deque;
import java.util.ArrayDeque;
//+END_SOLUTION

public class BellmanFordSPs {
    private static final double INFINITY = Double.POSITIVE_INFINITY;
    //+BEGIN_SOLUTION
    private DirectedEdge[] edgeTo;
    private double[] distTo;
    private Deque<Integer> queue;
    private boolean[] onQueue;
    private int counter;
    private Iterable<DirectedEdge> negCycle;
    //+END_SOLUTION

    public BellmanFordSPs(EdgeWeightedDigraph G, int s) {
        //+BEGIN_SOLUTION
        edgeTo = new DirectedEdge[G.V()];
        distTo = new double[G.V()];
        queue = new ArrayDeque<>();
        onQueue = new boolean[G.V()];

        for (int v = 0; v < G.V(); v++) {
            distTo[v] = INFINITY;
        }
        distTo[s] = 0.0;

        queue.add(s);
        onQueue[s] = true;

        while (!queue.isEmpty() && !hasNegCycle()) {
            int v = queue.remove();
            onQueue[v] = false;
            relax(G, v);
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
                if (!onQueue[w]) {
                    queue.add(w);
                    onQueue[w] = true;
                }
            }
            if (counter++ % G.V() == 0) {
                findNegCycle();
            }
        }
    }

    private void findNegCycle() {
        int V = edgeTo.length;
        EdgeWeightedDigraph spt = new EdgeWeightedDigraph(V);
        for (int v = 0; v < V; v++) {
            if (edgeTo[v] != null) {
                spt.addEdge(edgeTo[v]);
            }
        }
        EdgeWeightedDigraphCycle cf = new EdgeWeightedDigraphCycle(spt);
        negCycle = cf.cycle();
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
        Deque<DirectedEdge> path = new ArrayDeque<>();
        for (DirectedEdge e = edgeTo[v]; e != null; e = edgeTo[e.from()]) {
            path.push(e);
        }
        return path;
        //+END_SOLUTION
    }

    public boolean hasNegCycle() {
        //+BEGIN_SOLUTION
        return negCycle != null;
        //+END_SOLUTION
    }

    public Iterable<DirectedEdge> negCycle() {
        //+BEGIN_SOLUTION
        return negCycle;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/tinyEWDn.txt
        EdgeWeightedDigraph G = new EdgeWeightedDigraph(8);
        G.addEdge(new DirectedEdge(4, 5,  0.35));
        G.addEdge(new DirectedEdge(5, 4,  0.35));
        G.addEdge(new DirectedEdge(4, 7,  0.37));
        G.addEdge(new DirectedEdge(5, 7,  0.28));
        G.addEdge(new DirectedEdge(7, 5,  0.28));
        G.addEdge(new DirectedEdge(5, 1,  0.32));
        G.addEdge(new DirectedEdge(0, 4,  0.38));
        G.addEdge(new DirectedEdge(0, 2,  0.26));
        G.addEdge(new DirectedEdge(7, 3,  0.39));
        G.addEdge(new DirectedEdge(1, 3,  0.29));
        G.addEdge(new DirectedEdge(2, 7,  0.34));
        G.addEdge(new DirectedEdge(6, 2, -1.20));
        G.addEdge(new DirectedEdge(3, 6,  0.52));
        G.addEdge(new DirectedEdge(6, 0, -1.40));
        G.addEdge(new DirectedEdge(6, 4, -1.25));
        //System.out.println(G);
        //System.out.println(G.toDot());

        BellmanFordSPs sp0 = new BellmanFordSPs(G, 0);
        Assert.assertFalse(sp0.hasNegCycle());

        Assert.assertTrue(sp0.hasPathTo(0));
        Assert.assertEquals(0.0, sp0.distTo(0));
        Assert.assertEquals("", GraphUtil.directedWeightedPathToString(sp0.pathTo(0)));

        Assert.assertTrue(sp0.hasPathTo(1));
        Assert.assertEquals(0.93, sp0.distTo(1), 10e-4);
        Assert.assertEquals("0->2->7->3->6->4->5->1", GraphUtil.directedWeightedPathToString(sp0.pathTo(1)));

        Assert.assertTrue(sp0.hasPathTo(2));
        Assert.assertEquals(0.26, sp0.distTo(2), 10e-4);
        Assert.assertEquals("0->2", GraphUtil.directedWeightedPathToString(sp0.pathTo(2)));

        Assert.assertTrue(sp0.hasPathTo(3));
        Assert.assertTrue(sp0.hasPathTo(4));
        Assert.assertTrue(sp0.hasPathTo(5));

        Assert.assertTrue(sp0.hasPathTo(6));
        Assert.assertEquals(1.51, sp0.distTo(6), 10e-4);
        Assert.assertEquals("0->2->7->3->6", GraphUtil.directedWeightedPathToString(sp0.pathTo(6)));

        Assert.assertTrue(sp0.hasPathTo(7));

        // data/tinyEWDnc.txt
        G = new EdgeWeightedDigraph(8);
        G.addEdge(new DirectedEdge(4, 5,  0.35));
        G.addEdge(new DirectedEdge(5, 4, -0.66));
        G.addEdge(new DirectedEdge(4, 7,  0.37));
        G.addEdge(new DirectedEdge(5, 7,  0.28));
        G.addEdge(new DirectedEdge(7, 5,  0.28));
        G.addEdge(new DirectedEdge(5, 1,  0.32));
        G.addEdge(new DirectedEdge(0, 4,  0.38));
        G.addEdge(new DirectedEdge(0, 2,  0.26));
        G.addEdge(new DirectedEdge(7, 3,  0.39));
        G.addEdge(new DirectedEdge(1, 3,  0.29));
        G.addEdge(new DirectedEdge(2, 7,  0.34));
        G.addEdge(new DirectedEdge(6, 2,  0.40));
        G.addEdge(new DirectedEdge(3, 6,  0.52));
        G.addEdge(new DirectedEdge(6, 0,  0.58));
        G.addEdge(new DirectedEdge(6, 4,  0.93));
        //System.out.println(G);
        //System.out.println(G.toDot());

        sp0 = new BellmanFordSPs(G, 0);
        Assert.assertTrue(sp0.hasNegCycle());
        Assert.assertEquals("5->4->5", GraphUtil.directedWeightedPathToString(sp0.negCycle()));

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

public class EdgeWeightedDigraphCycle {
    public EdgeWeightedDigraphCycle(EdgeWeightedDigraph G);
    public Iterable<DirectedEdge> cycle();
}

+END_FOLD*/
