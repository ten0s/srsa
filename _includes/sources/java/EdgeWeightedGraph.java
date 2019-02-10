public class EdgeWeightedGraph {
    // BEGIN_SOLUTION
    private final int V;
    private int E;
    private Bag<Edge>[] adj;
    // END_SOLUTION

    @SuppressWarnings("unchecked")
    public EdgeWeightedGraph(int V) {
        // BEGIN_SOLUTION
        this.V = V;
        adj = (Bag<Edge>[]) new Bag[V];
        for (int v = 0; v < V; v++) {
            adj[v] = new Bag<>();
        }
        // END_SOLUTION
    }

    public int V() {
        // BEGIN_SOLUTION
        return V;
        // END_SOLUTION
    }

    public int E() {
        // BEGIN_SOLUTION
        return E;
        // END_SOLUTION
    }

    public void addEdge(Edge e) {
        // BEGIN_SOLUTION
        int v = e.either(), w = e.other(v);
        adj[v].add(e);
        adj[w].add(e);
        E++;
        // END_SOLUTION
    }

    public Iterable<Edge> adj(int v) {
        // BEGIN_SOLUTION
        return adj[v];
        // END_SOLUTION
    }

    public Iterable<Edge> edges() {
        // BEGIN_SOLUTION
        Bag<Edge> edges = new Bag<>();
        for (int v = 0; v < V; v++) {
            for (Edge e : adj[v]) {
                if (e.other(v) > v) {
                    edges.add(e);
                }
            }
        }
        return edges;
        // END_SOLUTION
    }

    public String toString() {
        String s = V + " vertices, " + E + " edges" + System.lineSeparator();
        for (Edge e : edges()) {
            s += e + System.lineSeparator();
        }
        return s;
    }

    public String toDot() {
        String s = "graph {" + System.lineSeparator();
        for (Edge e : edges()) {
            int v = e.either(), w = e.other(v);
            double weight = e.weight();
            s += v + " -- " + w + " " + label(weight) + ";" + System.lineSeparator();
        }
        s += "}" + System.lineSeparator();
        return s;
    }

    private String label(double weight) {
        return "[label=" + Character.toString('"') + weight + Character.toString('"') + "]";
    }

    public static void main(String[] args) throws Throwable {
        // /data/tinyEWG.txt
        EdgeWeightedGraph G = new EdgeWeightedGraph(8);
        Assert.assertEquals(8, G.V());
        Assert.assertEquals(0, G.E());

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

        Assert.assertEquals(8, G.V());
        Assert.assertEquals(16, G.E());
        int degree0 = 0; for (Edge e : G.adj(0)) degree0++;
        Assert.assertEquals(4, degree0);
        int degree3 = 0; for (Edge e : G.adj(3)) degree3++;
        Assert.assertEquals(3, degree3);
        int degree6 = 0; for (Edge e : G.adj(6)) degree6++;
        Assert.assertEquals(4, degree6);

        // the same set of edges as in /data/tinyEWG.txt
        HashSet<Edge> set = new HashSet<>();
        set.add(new Edge(4, 5, 0.35));
        set.add(new Edge(4, 7, 0.37));
        set.add(new Edge(5, 7, 0.28));
        set.add(new Edge(0, 7, 0.16));
        set.add(new Edge(1, 5, 0.32));
        set.add(new Edge(0, 4, 0.38));
        set.add(new Edge(2, 3, 0.17));
        set.add(new Edge(1, 7, 0.19));
        set.add(new Edge(0, 2, 0.26));
        set.add(new Edge(1, 2, 0.36));
        set.add(new Edge(1, 3, 0.29));
        set.add(new Edge(2, 7, 0.34));
        set.add(new Edge(6, 2, 0.40));
        set.add(new Edge(3, 6, 0.52));
        set.add(new Edge(6, 0, 0.58));
        set.add(new Edge(6, 4, 0.93));
        for (Edge e : G.edges()) {
            Assert.assertTrue(set.contains(e));
            set.delete(e);
        }
        Assert.assertTrue(set.isEmpty());

        System.out.println("OK");
    }
}

/*
public class Bag<Item> implements Iterable<Item> {
    public Bag();
    public void add(Item item);
}

public class Edge implements Comparable<Edge> {
    public Edge(int v, int w, double weight);
    public double weight();
    public int either();
    public int other(int vertex);
}
*/
