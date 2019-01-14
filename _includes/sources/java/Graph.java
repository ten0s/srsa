/*
public class Bag<Item> implements Iterable<Item> {
    public Bag();
    public void add(Item item);
}
*/

public class Graph {
    // SOLUTION_BEGIN
    private final int V;        // number of vertices
    private int E;              // number of edges
    private Bag<Integer>[] adj; // adjacency lists
    // SOLUTION_END

    @SuppressWarnings("unchecked")
    public Graph(int V) {
        // SOLUTION_BEGIN
        this.V = V;
        adj = (Bag<Integer>[]) new Bag[V];
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

    public void addEdge(int v, int w) {
        // SOLUTION_BEGIN
        if (v != w) {
            adj[v].add(w);
            adj[w].add(v);
        } else {
            // add self-loop once
            adj[v].add(v);
        }
        E++;
        // SOLUTION_END
    }

    public Iterable<Integer> adj(int v) {
        // SOLUTION_BEGIN
        return adj[v];
        // SOLUTION_END
    }

    public String toString() {
        String s = V + " vertices, " + E + " edges" + System.lineSeparator();
        for (int v = 0; v < V; v++) {
            s += v + ": ";
            for (int w : adj(v)) {
                s += w + " ";
            }
            s += System.lineSeparator();
        }
        return s;
    }

    public String toDot() {
        String s = "graph {" + System.lineSeparator();
        HashSet<Integer> nodes = new HashSet<>();
        HashSet<String> pairs = new HashSet<>();
        for (int v = 0; v < V; v++) {
            for (int w : adj(v)) {
                nodes.add(w);
                if (!pairs.contains(w + "-" + v)) {
                    s += "  " + v + " -- " + w + ";" + System.lineSeparator();
                    pairs.add(v + "-" + w);
                }
            }
        }
        for (int v = 0; v < V; v++) {
            if (!nodes.contains(v)) {
                s += "  " + v + System.lineSeparator();
            }
        }
        s += "}" + System.lineSeparator();
        return s;
    }

    public static void main(String[] args) throws Throwable {
        Graph G = new Graph(10);
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
        int degree0 = 0; for (int v : G.adj(0)) degree0++;
        Assert.assertEquals(2, degree0);
        int degree1 = 0; for (int v : G.adj(1)) degree1++;
        Assert.assertEquals(5, degree1);
        int degree8 = 0; for (int v : G.adj(8)) degree8++;
        Assert.assertEquals(4, degree8);

        System.out.println("OK");
    }
}