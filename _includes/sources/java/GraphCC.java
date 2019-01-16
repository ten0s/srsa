import java.util.Iterator;
import java.util.Collections;

public class GraphCC {
    // SOLUTION_BEGIN
    private boolean[] marked;
    private int[] id;
    private int count;
    // SOLUTION_END

    public GraphCC(Graph G) {
        // SOLUTION_BEGIN
        marked = new boolean[G.V()];
        id = new int[G.V()];
        for (int v = 0; v < G.V(); v++) {
            if (!marked[v]) {
                dfs(v, G);
                count++;
            }
        }
        // SOLUTION_END
    }

    private void dfs(int v, Graph G) {
        // SOLUTION_BEGIN
        marked[v] = true;
        id[v] = count;
        for (int w : G.adj(v)) {
            if (!marked[w]) {
                dfs(w, G);
            }
        }
        // SOLUTION_END
    }

    public boolean connected(int v, int w) {
        // SOLUTION_BEGIN
        return id(v) == id(w);
        // SOLUTION_END
    }

    public int id(int v) {
        // SOLUTION_BEGIN
        return id[v];
        // SOLUTION_END
    }

    public int count() {
        // SOLUTION_BEGIN
        return count;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        // /data/tinyG.txt
        Graph G = new Graph(13);
        G.addEdge(0, 5);
        G.addEdge(4, 3);
        G.addEdge(0, 1);
        G.addEdge(9, 12);
        G.addEdge(6, 4);
        G.addEdge(5, 4);
        G.addEdge(0, 2);
        G.addEdge(11, 12);
        G.addEdge(9, 10);
        G.addEdge(0, 6);
        G.addEdge(7, 8);
        G.addEdge(9, 11);
        G.addEdge(5, 3);

        GraphCC cc = new GraphCC(G);
        Assert.assertEquals(3, cc.count());

        // 0
        Assert.assertEquals(0, cc.id(0));
        for (int v = 1; v <= 6; v++) {
            Assert.assertTrue(cc.connected(0, v));
        }
        Assert.assertFalse(cc.connected(0, 7));
        Assert.assertFalse(cc.connected(0, 9));

        // 1
        Assert.assertEquals(1, cc.id(7));
        Assert.assertTrue(cc.connected(7, 8));
        Assert.assertFalse(cc.connected(7, 9));

        // 2
        Assert.assertEquals(2, cc.id(9));
        for (int v = 10; v <= 12; v++) {
            Assert.assertTrue(cc.connected(9, v));
        }

        System.out.println("OK");
    }
}
