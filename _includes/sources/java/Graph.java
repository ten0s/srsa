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
        adj[v].add(w);
        adj[w].add(v);
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
        HashSet<String> set = new HashSet<>();
        for (int v = 0; v < V; v++) {
            for (int w : adj(v)) {
                if (!set.contains(w + "-" + v)) {
                    s += "  " + v + " -- " + w + ";" + System.lineSeparator();
                    set.add(v + "-" + w);
                }
            }
        }
        s += "}" + System.lineSeparator();
        return s;
    }

    public static void main(String[] args) throws Throwable {
        Graph g = new Graph(10);
        Assert.assertEquals(10, g.V());
        Assert.assertEquals(0, g.E());

        g.addEdge(0, 1);
        g.addEdge(0, 7);
        g.addEdge(1, 2);
        g.addEdge(1, 3);
        g.addEdge(1, 7);
        g.addEdge(1, 8);
        g.addEdge(2, 3);
        g.addEdge(3, 4);
        g.addEdge(3, 5);
        g.addEdge(3, 8);
        g.addEdge(4, 5);
        g.addEdge(5, 6);
        g.addEdge(5, 7);
        g.addEdge(5, 8);
        g.addEdge(6, 7);
        g.addEdge(7, 8);
        //System.out.println(g.toDot());

        Assert.assertEquals(10, g.V());
        Assert.assertEquals(16, g.E());
        int degree0 = 0; for (int v : g.adj(0)) degree0++;
        Assert.assertEquals(2, degree0);
        int degree1 = 0; for (int v : g.adj(1)) degree1++;
        Assert.assertEquals(5, degree1);
        int degree8 = 0; for (int v : g.adj(8)) degree8++;
        Assert.assertEquals(4, degree8);

        System.out.println("OK");
    }
}
