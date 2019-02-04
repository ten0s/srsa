public class GraphBFSPaths {
    private static final int INFINITY = Integer.MAX_VALUE;
    // SOLUTION_BEGIN
    private boolean[] marked;
    private int[] edgeTo;
    private int[] distTo;
    private final int s;
    // SOLUTION_END

    public GraphBFSPaths(Graph G, int s) {
        // SOLUTION_BEGIN
        marked = new boolean[G.V()];
        edgeTo = new int[G.V()];
        distTo = new int[G.V()];
        for (int v = 0; v < G.V(); v++) {
            distTo[v] = INFINITY;
        }
        this.s = s;
        bfs(G, s);
        // SOLUTION_END
    }

    // SOLUTION_BEGIN
    private void bfs(Graph G, int s) {
        Queue<Integer> queue = new Queue<>();
        marked[s] = true;
        distTo[s] = 0;
        queue.enqueue(s);
        while (!queue.isEmpty()) {
            int v = queue.dequeue();
            for (int w : G.adj(v)) {
                if (!marked[w]) {
                    marked[w] = true;
                    edgeTo[w] = v;
                    distTo[w] = distTo[v] + 1;
                    queue.enqueue(w);
                }
            }
        }
    }
    // SOLUTION_END

    public boolean hasPathTo(int v) {
        // SOLUTION_BEGIN
        return marked[v];
        // SOLUTION_END
    }

    public Iterable<Integer> pathTo(int v) {
        // SOLUTION_BEGIN
        if (!hasPathTo(v)) {
            return null;
        }
        Stack<Integer> path = new Stack<>();
        for (int x = v; x != s; x = edgeTo[x]) {
            path.push(x);
        }
        path.push(s);
        return path;
        // SOLUTION_END
    }

    public int distTo(int v) {
        // SOLUTION_BEGIN
        return distTo[v];
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

        GraphBFSPaths ps0 = new GraphBFSPaths(G, 0);
        Assert.assertTrue(ps0.hasPathTo(0));
        Assert.assertEquals(0, ps0.distTo(0));

        Assert.assertTrue(ps0.hasPathTo(1));
        Assert.assertEquals(1, ps0.distTo(1));

        Assert.assertTrue(ps0.hasPathTo(2));
        Assert.assertEquals(1, ps0.distTo(2));

        Assert.assertTrue(ps0.hasPathTo(3));
        Assert.assertEquals(2, ps0.distTo(3));
        Assert.assertEquals("0-5-3", GraphUtil.pathToString(ps0.pathTo(3)));

        Assert.assertTrue(ps0.hasPathTo(4));
        Assert.assertEquals(2, ps0.distTo(4));
        Assert.assertEquals("0-6-4", GraphUtil.pathToString(ps0.pathTo(4)));

        Assert.assertTrue(ps0.hasPathTo(5));
        Assert.assertEquals(1, ps0.distTo(5));
        Assert.assertEquals("0-5", GraphUtil.pathToString(ps0.pathTo(5)));

        Assert.assertTrue(ps0.hasPathTo(6));
        Assert.assertEquals(1, ps0.distTo(6));

        Assert.assertFalse(ps0.hasPathTo(7));
        Assert.assertEquals(INFINITY, ps0.distTo(7));

        Assert.assertFalse(ps0.hasPathTo(9));
        Assert.assertEquals(INFINITY, ps0.distTo(9));

        GraphBFSPaths ps9 = new GraphBFSPaths(G, 9);
        Assert.assertTrue(ps9.hasPathTo(9));
        Assert.assertEquals(0, ps9.distTo(9));

        Assert.assertTrue(ps9.hasPathTo(10));
        Assert.assertEquals(1, ps9.distTo(10));

        Assert.assertTrue(ps9.hasPathTo(11));
        Assert.assertEquals(1, ps9.distTo(11));

        Assert.assertTrue(ps9.hasPathTo(12));
        Assert.assertEquals(1, ps9.distTo(12));

        Assert.assertFalse(ps9.hasPathTo(0));
        Assert.assertEquals(INFINITY, ps9.distTo(0));

        Assert.assertFalse(ps9.hasPathTo(7));
        Assert.assertEquals(INFINITY, ps9.distTo(7));

        System.out.println("OK");
    }
}

/*
public class public class Queue<Item> implements Iterable<Item> {
    public Queue();
    public void enqueue(Item item);
    public Item dequeue();
    public boolean isEmpty();
}

public class public class Stack<Item> implements Iterable<Item> {
    public Stack();
    public void push(Item item);
}
*/
