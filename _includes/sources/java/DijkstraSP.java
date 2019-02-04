public class DijkstraSP {
    private static final double INFINITY = Double.POSITIVE_INFINITY;
    // SOLUTION_BEGIN
    private DirectedEdge[] edgeTo;
    private double[] distTo;
    private IndexMinPQ<Double> pq;
    // SOLUTION_END

    public DijkstraSP(EdgeWeightedDigraph G, int s) {
        // SOLUTION_BEGIN
        edgeTo = new DirectedEdge[G.V()];
        distTo = new double[G.V()];
        pq = new IndexMinPQ<>(G.V());

        for (int v = 0; v < G.V(); v++) {
            distTo[v] = INFINITY;
        }
        distTo[s] = 0.0;

        pq.insert(s, 0.0);
        while (!pq.isEmpty()) {
            relax(G, pq.delMin());
        }
        // SOLUTION_END
    }

    // SOLUTION_BEGIN
    private void relax(EdgeWeightedDigraph G, int v) {
        for (DirectedEdge e : G.adj(v)) {
            int w = e.to();
            if (distTo[w] > distTo[v] + e.weight()) {
                distTo[w] = distTo[v] + e.weight();
                edgeTo[w] = e;
                if (pq.contains(w)) pq.changeKey(w, distTo[w]);
                else                pq.insert(w, distTo[w]);
            }
        }
    }
    // SOLUTION_END

    public double distTo(int v) {
        // SOLUTION_BEGIN
        return distTo[v];
        // SOLUTION_END
    }

    public boolean hasPathTo(int v) {
        // SOLUTION_BEGIN
        return distTo[v] < INFINITY;
        // SOLUTION_END
    }

    public Iterable<DirectedEdge> pathTo(int v) {
        // SOLUTION_BEGIN
        if (!hasPathTo(v)) return null;
        Stack<DirectedEdge> path = new Stack<>();
        for (DirectedEdge e = edgeTo[v]; e != null; e = edgeTo[e.from()]) {
            path.push(e);
        }
        return path;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        // /data/tinyEWG.txt
        EdgeWeightedDigraph G = new EdgeWeightedDigraph(8);
        G.addEdge(new DirectedEdge(4, 5, 0.35));
        G.addEdge(new DirectedEdge(4, 7, 0.37));
        G.addEdge(new DirectedEdge(5, 7, 0.28));
        G.addEdge(new DirectedEdge(0, 7, 0.16));
        G.addEdge(new DirectedEdge(1, 5, 0.32));
        G.addEdge(new DirectedEdge(0, 4, 0.38));
        G.addEdge(new DirectedEdge(2, 3, 0.17));
        G.addEdge(new DirectedEdge(1, 7, 0.19));
        G.addEdge(new DirectedEdge(0, 2, 0.26));
        G.addEdge(new DirectedEdge(1, 2, 0.36));
        G.addEdge(new DirectedEdge(1, 3, 0.29));
        G.addEdge(new DirectedEdge(2, 7, 0.34));
        G.addEdge(new DirectedEdge(6, 2, 0.40));
        G.addEdge(new DirectedEdge(3, 6, 0.52));
        G.addEdge(new DirectedEdge(6, 0, 0.58));
        G.addEdge(new DirectedEdge(6, 4, 0.93));
        //System.out.println(G);
        //System.out.println(G.toDot());

        DijkstraSP sp0 = new DijkstraSP(G, 0);
        Assert.assertTrue(sp0.hasPathTo(0));
        Assert.assertEquals(0.0, sp0.distTo(0));
        Assert.assertEquals("", GraphUtil.directedWeightedPathToString(sp0.pathTo(0)));

        Assert.assertFalse(sp0.hasPathTo(1));
        Assert.assertEquals(INFINITY, sp0.distTo(1));
        Assert.assertNull(sp0.pathTo(1));

        Assert.assertTrue(sp0.hasPathTo(2));
        Assert.assertEquals(0.26, sp0.distTo(2), 10e-4);
        Assert.assertEquals("0->2", GraphUtil.directedWeightedPathToString(sp0.pathTo(2)));

        Assert.assertTrue(sp0.hasPathTo(3));
        Assert.assertTrue(sp0.hasPathTo(4));
        Assert.assertTrue(sp0.hasPathTo(5));

        Assert.assertTrue(sp0.hasPathTo(6));
        Assert.assertEquals(0.95, sp0.distTo(6), 10e-4);
        Assert.assertEquals("0->2->3->6", GraphUtil.directedWeightedPathToString(sp0.pathTo(6)));

        Assert.assertTrue(sp0.hasPathTo(7));

        System.out.println("OK");
    }
}

/*
public class DirectedEdge implements Comparable<DirectedEdge> {
    public DirectedEdge(int v, int w, double weight);
    public double weight();
    public int from();
    public int to();
}

class IndexMinPQ<Key extends Comparable<Key>> {
    public IndexMinPQ();
    public boolean contains(int i);
    public void insert(int i, Key v);
    public void changeKey(int i, key v);
    public int delMin();
    public int size();
    public boolean isEmpty();
}

public class public class Stack<Item> implements Iterable<Item> {
    public Stack();
    public void push(Item item);
}
*/
