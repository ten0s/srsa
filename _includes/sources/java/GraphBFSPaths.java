import java.util.*;

public class GraphBFSPaths {
    private static final int INFINITY = Integer.MAX_VALUE;
    //+BEGIN_SOLUTION
    private boolean[] marked;
    private int[] edgeTo;
    private int[] distTo;
    private final int s;
    //+END_SOLUTION

    public GraphBFSPaths(Graph G, int s) {
        //+BEGIN_SOLUTION
        marked = new boolean[G.V()];
        edgeTo = new int[G.V()];
        distTo = new int[G.V()];
        for (int v = 0; v < G.V(); v++) {
            distTo[v] = INFINITY;
        }
        this.s = s;
        bfs(G, s);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private void bfs(Graph G, int s) {
        Deque<Integer> queue = new ArrayDeque<>();
        marked[s] = true;
        distTo[s] = 0;
        queue.add(s);
        while (!queue.isEmpty()) {
            int v = queue.remove();
            for (int w : G.adj(v)) {
                if (!marked[w]) {
                    marked[w] = true;
                    edgeTo[w] = v;
                    distTo[w] = distTo[v] + 1;
                    queue.add(w);
                }
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
        if (!hasPathTo(v)) {
            return null;
        }
        Deque<Integer> path = new ArrayDeque<>();
        for (int x = v; x != s; x = edgeTo[x]) {
            path.push(x);
        }
        path.push(s);
        return path;
        //+END_SOLUTION
    }

    public int distTo(int v) {
        //+BEGIN_SOLUTION
        return distTo[v];
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/tinyG.txt
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

        GraphBFSPaths sp0 = new GraphBFSPaths(G, 0);
        Assert.assertTrue(sp0.hasPathTo(0));
        Assert.assertEquals(0, sp0.distTo(0));

        Assert.assertTrue(sp0.hasPathTo(1));
        Assert.assertEquals(1, sp0.distTo(1));

        Assert.assertTrue(sp0.hasPathTo(2));
        Assert.assertEquals(1, sp0.distTo(2));

        Assert.assertTrue(sp0.hasPathTo(3));
        Assert.assertEquals(2, sp0.distTo(3));
        Assert.assertEquals("0-5-3", GraphUtil.pathToString(sp0.pathTo(3)));

        Assert.assertTrue(sp0.hasPathTo(4));
        Assert.assertEquals(2, sp0.distTo(4));
        Assert.assertEquals("0-5-4", GraphUtil.pathToString(sp0.pathTo(4)));

        Assert.assertTrue(sp0.hasPathTo(5));
        Assert.assertEquals(1, sp0.distTo(5));
        Assert.assertEquals("0-5", GraphUtil.pathToString(sp0.pathTo(5)));

        Assert.assertTrue(sp0.hasPathTo(6));
        Assert.assertEquals(1, sp0.distTo(6));

        Assert.assertFalse(sp0.hasPathTo(7));
        Assert.assertEquals(INFINITY, sp0.distTo(7));

        Assert.assertFalse(sp0.hasPathTo(9));
        Assert.assertEquals(INFINITY, sp0.distTo(9));

        GraphBFSPaths sp9 = new GraphBFSPaths(G, 9);
        Assert.assertTrue(sp9.hasPathTo(9));
        Assert.assertEquals(0, sp9.distTo(9));

        Assert.assertTrue(sp9.hasPathTo(10));
        Assert.assertEquals(1, sp9.distTo(10));

        Assert.assertTrue(sp9.hasPathTo(11));
        Assert.assertEquals(1, sp9.distTo(11));

        Assert.assertTrue(sp9.hasPathTo(12));
        Assert.assertEquals(1, sp9.distTo(12));

        Assert.assertFalse(sp9.hasPathTo(0));
        Assert.assertEquals(INFINITY, sp9.distTo(0));

        Assert.assertFalse(sp9.hasPathTo(7));
        Assert.assertEquals(INFINITY, sp9.distTo(7));

        System.out.println("OK");
    }
    //+END_FOLD }
}
