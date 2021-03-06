import java.util.*;

public class LazyPrimMST {
    //+BEGIN_SOLUTION
    private boolean[] marked;
    private PriorityQueue<Edge> pq;
    private Deque<Edge> mst;
    private double weight;
    //+END_SOLUTION

    public LazyPrimMST(EdgeWeightedGraph G) {
        //+BEGIN_SOLUTION
        marked = new boolean[G.V()];
        pq = new PriorityQueue<>();
        mst = new ArrayDeque<>();

        visit(G, 0);
        while (!pq.isEmpty() && mst.size() < G.V() - 1) {
            Edge e = pq.remove();
            int v = e.either(), w = e.other(v);
            if (marked[v] && marked[w]) continue;
            mst.add(e);
            weight += e.weight();
            if (!marked[v]) visit(G, v);
            if (!marked[w]) visit(G, w);
        }
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private void visit(EdgeWeightedGraph G, int v) {
        marked[v] = true;
        for (Edge e : G.adj(v)) {
            if (!marked[e.other(v)]) {
                pq.add(e);
            }
        }
    }
    //+END_SOLUTION

    public Iterable<Edge> edges() {
        //+BEGIN_SOLUTION
        return mst;
        //+END_SOLUTION
    }

    public double weight() {
        //+BEGIN_SOLUTION
        return weight;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/tinyEWG.txt
        EdgeWeightedGraph G = new EdgeWeightedGraph(8);
        G.addEdge(new Edge(4, 5, 0.35));
        G.addEdge(new Edge(4, 7, 0.37));
        G.addEdge(new Edge(5, 7, 0.28));
        G.addEdge(new Edge(0, 7, 0.16));
        G.addEdge(new Edge(1, 5, 0.32));
        G.addEdge(new Edge(0, 4, 0.38));
        G.addEdge(new Edge(2, 3, 0.17));
        G.addEdge(new Edge(1, 7, 0.19));
        G.addEdge(new Edge(0, 2, 0.26));
        G.addEdge(new Edge(1, 2, 0.36));
        G.addEdge(new Edge(1, 3, 0.29));
        G.addEdge(new Edge(2, 7, 0.34));
        G.addEdge(new Edge(6, 2, 0.40));
        G.addEdge(new Edge(3, 6, 0.52));
        G.addEdge(new Edge(6, 0, 0.58));
        G.addEdge(new Edge(6, 4, 0.93));
        //System.out.println(G);
        //System.out.println(G.toDot());

        HashSet<Edge> set = new HashSet<>();
        set.add(new Edge(0, 7, 0.16));
        set.add(new Edge(2, 3, 0.17));
        set.add(new Edge(1, 7, 0.19));
        set.add(new Edge(0, 2, 0.26));
        set.add(new Edge(5, 7, 0.28));
        set.add(new Edge(4, 5, 0.35));
        set.add(new Edge(6, 2, 0.40));

        LazyPrimMST mst = new LazyPrimMST(G);
        for (Edge e : mst.edges()) {
            Assert.assertTrue(set.contains(e));
            set.remove(e);
        }
        Assert.assertTrue(set.isEmpty());

        Assert.assertEquals(1.81, mst.weight());

        System.out.println("OK");
    }
    //+END_FOLD }
}
