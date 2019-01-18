public class GraphBipartite {
    // SOLUTION_BEGIN
    private boolean[] marked;
    private boolean[] color;
    private boolean isBipartite = true;
    // SOLUTION_END

    public GraphBipartite(Graph G) {
        // SOLUTION_BEGIN
        marked = new boolean[G.V()];
        color = new boolean[G.V()];
        for (int v = 0; v < G.V(); v++) {
            if (!marked[v]) {
                dfs(G, v);
            }
        }
        // SOLUTION_END
    }

    private void dfs(Graph G, int v) {
        // SOLUTION_BEGIN
        marked[v] = true;
        for (int w : G.adj(v)) {
            if (!marked[w]) {
                color[w] = !color[v];
                dfs(G, w);
            } else {
                if (color[w] == color[v]) {
                    isBipartite = false;
                }
            }
        }
        // SOLUTION_END
    }

    public boolean isBipartite() {
        // SOLUTION_BEGIN
        return isBipartite;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        // /data/graph1.txt
        Graph G = new Graph(10);
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
        GraphBipartite b = new GraphBipartite(G);
        Assert.assertFalse(b.isBipartite());

        // /data/graph4.txt
        // /data/graph-bipartite.txt
        Graph G4 = new Graph(7);
        G4.addEdge(0, 1);
        G4.addEdge(0, 2);
        G4.addEdge(0, 5);
        G4.addEdge(0, 6);
        G4.addEdge(1, 3);
        G4.addEdge(2, 3);
        G4.addEdge(2, 4);
        G4.addEdge(4, 5);
        G4.addEdge(4, 6);
        GraphBipartite b4 = new GraphBipartite(G4);
        Assert.assertTrue(b4.isBipartite());

        System.out.println("OK");
    }
}
