/*
public class Bag<Item> implements Iterable<Item> {
    public Bag();
    public void add(Item item);
}

public class DirectedEdge implements Comparable<DirectedEdge> {
    public DirectedEdge(int v, int w, double weight);
    public double weight();
    public int from();
    public int to();
}
*/

public class EdgeWeightedDigraph {
    // SOLUTION_BEGIN
    private final int V;
    private int E;
    private Bag<DirectedEdge>[] adj;
    // SOLUTION_END

    @SuppressWarnings("unchecked")
    public EdgeWeightedDigraph(int V) {
        // SOLUTION_BEGIN
        this.V = V;
        adj = (Bag<DirectedEdge>[]) new Bag[V];
        for (int v = 0; v < V; v++) {
            adj[v] = new Bag<>();
        }
        // SOLUTION_END
    }

    public int V() {
        // SOLUTION_BEGIN
        return V;
        // SOLUTION_END
    }

    public int E() {
        // SOLUTION_BEGIN
        return E;
        // SOLUTION_END
    }

    public void addEdge(DirectedEdge e) {
        // SOLUTION_BEGIN
        adj[e.from()].add(e);
        E++;
        // SOLUTION_END
    }

    public Iterable<DirectedEdge> adj(int v) {
        // SOLUTION_BEGIN
        return adj[v];
        // SOLUTION_END
    }

    public Iterable<DirectedEdge> edges() {
        // SOLUTION_BEGIN
        Bag<DirectedEdge> edges = new Bag<>();
        for (int v = 0; v < V; v++) {
            for (DirectedEdge e : adj[v]) {
                edges.add(e);
            }
        }
        return edges;
        // SOLUTION_END
    }

    public EdgeWeightedDigraph reverse() {
        // SOLUTION_BEGIN
        EdgeWeightedDigraph R = new EdgeWeightedDigraph(V);
        for (int v = 0; v < V; v++) {
            for (DirectedEdge e : adj[v]) {
                R.addEdge(new DirectedEdge(e.to(), e.from(), e.weight()));
            }
        }
        return R;
        // SOLUTION_END
    }

    public String toString() {
        String s = V + " vertices, " + E + " edges" + System.lineSeparator();
        for (DirectedEdge e : edges()) {
            s += e + System.lineSeparator();
        }
        return s;
    }

    public String toDot() {
        String s = "digraph {" + System.lineSeparator();
        for (DirectedEdge e : edges()) {
            int v = e.from(), w = e.to();
            double weight = e.weight();
            s += v + " -> " + w + " " + label(weight) + ";" + System.lineSeparator();
        }
        s += "}" + System.lineSeparator();
        return s;
    }

    private String label(double weight) {
        return "[label=" + Character.toString('"') + weight + Character.toString('"') + "]";
    }

    public static void main(String[] args) throws Throwable {
        // /data/tinyEWG.txt
        EdgeWeightedDigraph G = new EdgeWeightedDigraph(8);
        Assert.assertEquals(8, G.V());
        Assert.assertEquals(0, G.E());

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

        Assert.assertEquals(8, G.V());
        Assert.assertEquals(16, G.E());
        int outdegree0 = 0; for (DirectedEdge e : G.adj(0)) outdegree0++;
        Assert.assertEquals(3, outdegree0);
        int outdegree3 = 0; for (DirectedEdge e : G.adj(3)) outdegree3++;
        Assert.assertEquals(1, outdegree3);
        int outdegree6 = 0; for (DirectedEdge e : G.adj(6)) outdegree6++;
        Assert.assertEquals(3, outdegree6);

        // the same set of edges as in /data/tinyEWG.txt
        HashSet<DirectedEdge> set = new HashSet<>();
        set.add(new DirectedEdge(4, 5, 0.35));
        set.add(new DirectedEdge(4, 7, 0.37));
        set.add(new DirectedEdge(5, 7, 0.28));
        set.add(new DirectedEdge(0, 7, 0.16));
        set.add(new DirectedEdge(1, 5, 0.32));
        set.add(new DirectedEdge(0, 4, 0.38));
        set.add(new DirectedEdge(2, 3, 0.17));
        set.add(new DirectedEdge(1, 7, 0.19));
        set.add(new DirectedEdge(0, 2, 0.26));
        set.add(new DirectedEdge(1, 2, 0.36));
        set.add(new DirectedEdge(1, 3, 0.29));
        set.add(new DirectedEdge(2, 7, 0.34));
        set.add(new DirectedEdge(6, 2, 0.40));
        set.add(new DirectedEdge(3, 6, 0.52));
        set.add(new DirectedEdge(6, 0, 0.58));
        set.add(new DirectedEdge(6, 4, 0.93));
        for (DirectedEdge e : G.edges()) {
            Assert.assertTrue(set.contains(e));
            set.delete(e);
        }
        Assert.assertTrue(set.isEmpty());

        EdgeWeightedDigraph R = G.reverse();
        //System.out.println(R);
        //System.out.println(R.toDot());
        Assert.assertEquals(8, R.V());
        Assert.assertEquals(16, R.E());
        outdegree0 = 0; for (DirectedEdge e : R.adj(0)) outdegree0++;
        Assert.assertEquals(1, outdegree0);
        outdegree3 = 0; for (DirectedEdge e : R.adj(3)) outdegree3++;
        Assert.assertEquals(2, outdegree3);
        outdegree6 = 0; for (DirectedEdge e : R.adj(6)) outdegree6++;
        Assert.assertEquals(1, outdegree6);

        System.out.println("OK");
    }
}
