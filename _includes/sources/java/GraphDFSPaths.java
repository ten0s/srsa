//+BEGIN_SOLUTION
import java.util.Deque;
import java.util.ArrayDeque;
//+END_SOLUTION

public class GraphDFSPaths {
    //+BEGIN_SOLUTION
    private boolean[] marked;
    private int[] edgeTo;
    private final int s;
    //+END_SOLUTION

    public GraphDFSPaths(Graph G, int s) {
        //+BEGIN_SOLUTION
        marked = new boolean[G.V()];
        edgeTo = new int[G.V()];
        this.s = s;
        dfs(G, s);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private void dfs(Graph G, int v) {
        marked[v] = true;
        for (int w : G.adj(v)) {
            if (!marked[w]) {
                edgeTo[w] = v;
                dfs(G, w);
            }
        }
    }
    //+END_SOLUTION

    public boolean hasPathTo(int v) {
        //+BEGIN_SOLUTION
        return marked[v];
        //+END_SOLUTION
    }

    public Iterable<Integer> pathTo(int v) {
        //+BEGIN_SOLUTION
        if (!hasPathTo(v)) return null;
        Deque<Integer> path = new ArrayDeque<>();
        for (int x = v; x != s; x = edgeTo[x]) {
            path.push(x);
        }
        path.push(s);
        return path;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
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

        GraphDFSPaths sp0 = new GraphDFSPaths(G, 0);
        Assert.assertTrue(sp0.hasPathTo(0));
        Assert.assertTrue(sp0.hasPathTo(1));
        Assert.assertTrue(sp0.hasPathTo(2));
        Assert.assertTrue(sp0.hasPathTo(3));
        Assert.assertTrue(sp0.hasPathTo(4));
        Assert.assertTrue(sp0.hasPathTo(5));
        Assert.assertTrue(sp0.hasPathTo(6));

        int l = 0;
        for (int x : sp0.pathTo(6)) { l++; }
        Assert.assertEquals(4, l);
        Assert.assertEquals("0-5-4-6", GraphUtil.pathToString(sp0.pathTo(6)));

        Assert.assertFalse(sp0.hasPathTo(7));
        Assert.assertFalse(sp0.hasPathTo(9));

        GraphDFSPaths sp9 = new GraphDFSPaths(G, 9);
        Assert.assertTrue(sp9.hasPathTo(9));
        Assert.assertTrue(sp9.hasPathTo(10));
        Assert.assertTrue(sp9.hasPathTo(11));
        Assert.assertTrue(sp9.hasPathTo(12));
        Assert.assertFalse(sp9.hasPathTo(0));
        Assert.assertFalse(sp9.hasPathTo(7));

        System.out.println("OK");
    }
    //+END_FOLD }
}
