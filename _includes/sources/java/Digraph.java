//+BEGIN_FOLD Tests {
import java.util.HashSet;
//+END_FOLD }
//+BEGIN_SOLUTION
import java.util.List;
import java.util.ArrayList;
//+END_SOLUTION

public class Digraph {
    //+BEGIN_SOLUTION
    private final int V;         // number of vertices
    private int E ;              // number of edges
    private List<Integer>[] adj; // adjacency lists
    //+END_SOLUTION

    @SuppressWarnings("unchecked")
    public Digraph(int V) {
        //+BEGIN_SOLUTION
        this.V = V;
        adj = (List<Integer>[]) new ArrayList[V];
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

    public void addEdge(int v, int w) {
        //+BEGIN_SOLUTION
        adj[v].add(w);
        E++;
        //+END_SOLUTION
    }

    public Iterable<Integer> adj(int v) {
        //+BEGIN_SOLUTION
        return adj[v];
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Utils {
    public String toString() {
        String s = V + " vertices, " + E + " edges" + System.lineSeparator();
        for (int v = 0; v < V; v++) {
            s += v + ": ";
            for (int w : adj[v]) {
                s += w + " ";
            }
            s += System.lineSeparator();
        }
        return s;
    }

    public String toDot() {
        String s = "digraph {" + System.lineSeparator();
        HashSet<Integer> nodes = new HashSet<>();
        for (int v = 0; v < V; v++) {
            for (int w : adj[v]) {
                nodes.add(v);
                nodes.add(w);
                s += "  " + v + " -> " + w + ";" + System.lineSeparator();
            }
        }
        for (int v = 0; v < V; v++) {
            if (!nodes.contains(v)) {
                s += "  " + v + ";" + System.lineSeparator();
            }
        }
        s += "}" + System.lineSeparator();
        return s;
    }
    //+END_FOLD }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/graph1.txt
        Digraph G = new Digraph(10);
        Assert.assertEquals(10, G.V());
        Assert.assertEquals(0, G.E());

        G.addEdge(0, 1);
        G.addEdge(0, 7);
        G.addEdge(1, 2);
        G.addEdge(1, 3);
        G.addEdge(1, 7);
        G.addEdge(1, 8);
        G.addEdge(2, 3);
        G.addEdge(3, 4);
        G.addEdge(3, 5);
        G.addEdge(3, 8);
        G.addEdge(4, 5);
        G.addEdge(5, 6);
        G.addEdge(5, 7);
        G.addEdge(5, 8);
        G.addEdge(6, 7);
        G.addEdge(7, 8);
        //System.out.println(G);
        //System.out.println(G.toDot());

        Assert.assertEquals(10, G.V());
        Assert.assertEquals(16, G.E());
        int outdegree0 = 0; for (int v : G.adj(0)) outdegree0++;
        Assert.assertEquals(2, outdegree0);
        int outdegree1 = 0; for (int v : G.adj(1)) outdegree1++;
        Assert.assertEquals(4, outdegree1);
        int outdegree8 = 0; for (int v : G.adj(8)) outdegree8++;
        Assert.assertEquals(0, outdegree8);

        System.out.println("OK");
    }
    //+END_FOLD }
}
