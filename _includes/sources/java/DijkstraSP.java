import java.util.*;

public class DijkstraSP {
    //+BEGIN_SOLUTION
    private DirectedEdge[] edgeTo;
    private double[] distTo;
    private IndexMinPQ<Double> pq;
    private final int t;
    //+END_SOLUTION

    public DijkstraSP(EdgeWeightedDigraph G, int s, int t) {
        //+BEGIN_SOLUTION
        edgeTo = new DirectedEdge[G.V()];
        distTo = new double[G.V()];
        pq = new IndexMinPQ<>(G.V());
        this.t = t;

        for (int v = 0; v < G.V(); v++) {
            distTo[v] = Double.POSITIVE_INFINITY;
        }
        distTo[s] = 0.0;

        pq.add(s, distTo[s]);
        while (!pq.isEmpty()) {
            int v = pq.remove();
            if (v == t) return;
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
                if (pq.contains(w)) pq.changeKey(w, distTo[w]);
                else                pq.add(w, distTo[w]);
            }
        }
    }
    //+END_SOLUTION

    public double dist() {
        //+BEGIN_SOLUTION
        return distTo[t];
        //+END_SOLUTION
    }

    public boolean hasPath() {
        //+BEGIN_SOLUTION
        return distTo[t] < Double.POSITIVE_INFINITY;
        //+END_SOLUTION
    }

    public Iterable<DirectedEdge> path() {
        //+BEGIN_SOLUTION
        if (!hasPath()) return null;
        Deque<DirectedEdge> path = new ArrayDeque<>();
        for (DirectedEdge e = edgeTo[t]; e != null; e = edgeTo[e.from()]) {
            path.push(e);
        }
        return path;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/tinyEWG.txt
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

        DijkstraSP sp00 = new DijkstraSP(G, 0, 0);
        Assert.assertTrue(sp00.hasPath());
        Assert.assertEquals(0.0, sp00.dist());
        Assert.assertEquals("", GraphUtil.directedWeightedPathToString(sp00.path()));

        DijkstraSP sp01 = new DijkstraSP(G, 0, 1);
        Assert.assertFalse(sp01.hasPath());
        Assert.assertEquals(Double.POSITIVE_INFINITY, sp01.dist());
        Assert.assertNull(sp01.path());

        DijkstraSP sp02 = new DijkstraSP(G, 0, 2);
        Assert.assertTrue(sp02.hasPath());
        Assert.assertEquals(0.26, sp02.dist(), 10e-4);
        Assert.assertEquals("0->2", GraphUtil.directedWeightedPathToString(sp02.path()));

        DijkstraSP sp06 = new DijkstraSP(G, 0, 6);
        Assert.assertTrue(sp06.hasPath());
        Assert.assertEquals(0.95, sp06.dist(), 10e-4);
        Assert.assertEquals("0->2->3->6", GraphUtil.directedWeightedPathToString(sp06.path()));

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

class IndexMinPQ<Key extends Comparable<Key>> {
    public IndexMinPQ();
    public boolean contains(int i);
    public void add(int i, Key v);
    public void changeKey(int i, key v);
    public int remove();
    public boolean isEmpty();
}
+END_FOLD*/
