import java.util.Iterator;
import java.util.Collections;

public class GraphCycle {
    // SOLUTION_BEGIN
    private boolean[] marked;
    private boolean hasCycle;
    // SOLUTION_END

    public GraphCycle(Graph G) {
        // SOLUTION_BEGIN
        marked = new boolean[G.V()];
        for (int v = 0; v < G.V(); v++) {
            if (!marked[v]) {
                dfs(-1, v, G);
            }
        }
        // SOLUTION_END
    }

    public boolean hasCycle() {
        // SOLUTION_BEGIN
        return hasCycle;
        // SOLUTION_END
    }

    private void dfs(int u, int v, Graph G) {
        // SOLUTION_BEGIN
        marked[v] = true;
        for (int w : G.adj(v)) {
            if (!marked[w]) {
                dfs(v, w, G);
            } else {
                // if vertex is marked and
                // is NOT our grandparent
                if (w != u) {
                    hasCycle = true;
                }
            }
        }
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        // /sources/data/graph3.txt
        Graph G3 = new Graph(6);
        G3.addEdge(0, 2);
        G3.addEdge(1, 2);
        G3.addEdge(2, 3);
        G3.addEdge(3, 4);
        G3.addEdge(3, 5);
        GraphCycle c3 = new GraphCycle(G3);
        Assert.assertFalse(c3.hasCycle());

        // /sources/data/graph4.txt
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
        GraphCycle c4 = new GraphCycle(G4);
        Assert.assertTrue(c4.hasCycle());

        System.out.println("OK");
    }
}
