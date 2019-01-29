/*
public class public class Queue<Item> implements Iterable<Item> {
    public Queue();
    public void enqueue(Item item);
    public int size();
}

class MinPQ<Key extends Comparable<Key>> {
    public MinPQ();
    public void insert(Key v);
    public Key delMin();
    public boolean isEmpty();
}

public class UnionFind {
    public UnionFind(int n);
    public boolean connected(int p, int q);
    public void union(int p, int q);
}
*/

public class KruskalMST {
    // SOLUTION_BEGIN
    private Queue<Edge> mst;
    private double weight;
    // SOLUTION_END

    public KruskalMST(EdgeWeightedGraph G) {
        // SOLUTION_BEGIN
        mst = new Queue<>();

        MinPQ<Edge> pq = new MinPQ<>();
        for (Edge e : G.edges()) {
            pq.insert(e);
        }

        UnionFind uf = new UnionFind(G.V());

        while (!pq.isEmpty() && mst.size() < G.V() - 1) {
            Edge e = pq.delMin();
            int v = e.either(), w = e.other(v);
            if (!uf.connected(v, w)) {
                uf.union(v, w);
                mst.enqueue(e);
                weight += e.weight();
            }
        }
        // SOLUTION_END
    }

    public Iterable<Edge> edges() {
        // SOLUTION_BEGIN
        return mst;
        // SOLUTION_END
    }

    public double weight() {
        // SOLUTION_BEGIN
        return weight;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        // /data/tinyEWG.txt
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

        KruskalMST mst = new KruskalMST(G);
        for (Edge e : mst.edges()) {
            Assert.assertTrue(set.contains(e));
            set.delete(e);
        }
        Assert.assertTrue(set.isEmpty());

        Assert.assertEquals(1.81, mst.weight());

        System.out.println("OK");
    }
}
