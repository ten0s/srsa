import java.util.*;

public class EdgeWeightedDigraph {
    //+BEGIN_SOLUTION
    private final int V;
    private int E;
    private List<DirectedEdge>[] adj;
    //+END_SOLUTION

    @SuppressWarnings("unchecked")
    public EdgeWeightedDigraph(int V) {
        //+BEGIN_SOLUTION
        this.V = V;
        adj = (List<DirectedEdge>[]) new ArrayList[V];
        for (int v = 0; v < V; v++) {
            adj[v] = new ArrayList<>();
        }
        //+END_SOLUTION
    }

    public int V() {
        //+BEGIN_SOLUTION
        return V;
        //+END_SOLUTION
    }

    public int E() {
        //+BEGIN_SOLUTION
        return E;
        //+END_SOLUTION
    }

    public void addEdge(DirectedEdge e) {
        //+BEGIN_SOLUTION
        adj[e.from()].add(e);
        E++;
        //+END_SOLUTION
    }

    public Iterable<DirectedEdge> adj(int v) {
        //+BEGIN_SOLUTION
        return adj[v];
        //+END_SOLUTION
    }

    public Iterable<DirectedEdge> edges() {
        //+BEGIN_SOLUTION
        List<DirectedEdge> edges = new ArrayList<>();
        for (int v = 0; v < V; v++) {
            for (DirectedEdge e : adj[v]) {
                edges.add(e);
            }
        }
        return edges;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Utils {
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
    //+END_FOLD }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/tinyEWG.txt
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

        // the same set of edges as in data/tinyEWG.txt
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
            set.remove(e);
        }
        Assert.assertTrue(set.isEmpty());

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
+END_FOLD*/
