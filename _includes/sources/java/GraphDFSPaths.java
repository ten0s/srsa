import java.util.Iterator;
import java.util.Collections;

/*
public class public class Stack<Item> implements Iterable<Item> {
    public Stack();
    public void push(Item item);
}
*/

public class GraphDFSPaths {
    // SOLUTION_BEGIN
    private boolean[] marked;
    private int[] edgeTo;
    private final int s;
    // SOLUTION_END

    public GraphDFSPaths(int s, Graph G) {
        // SOLUTION_BEGIN
        marked = new boolean[G.V()];
        edgeTo = new int[G.V()];
        this.s = s;
        dfs(s, G);
        // SOLUTION_END
    }

    private void dfs(int v, Graph G) {
        // SOLUTION_BEGIN
        marked[v] = true;
        for (int w : G.adj(v)) {
            if (!marked[w]) {
                edgeTo[w] = v;
                dfs(w, G);
            }
        }
        // SOLUTION_END
    }

    public boolean hasPathTo(int v) {
        // SOLUTION_BEGIN
        return marked[v];
        // SOLUTION_END
    }

    public Iterable<Integer> pathTo(int v) {
        // SOLUTION_BEGIN
        if (!hasPathTo(v)) {
            return new Iterable<Integer>() {
                public Iterator<Integer> iterator() {
                    return Collections.emptyIterator();
                }
            };
        }
        Stack<Integer> path = new Stack<>();
        for (int x = v; x != s; x = edgeTo[x]) {
            path.push(x);
        }
        path.push(s);
        return path;
        // SOLUTION_END
    }

    private static String pathToString(int v, GraphDFSPaths ps) {
        String s = "";
        for (int x : ps.pathTo(v)) {
            if (s.equals("")) {
                s = "" + x;
            } else {
                s += "-" + x;
            }
        }
        return s;
    }

    public static void main(String[] args) throws Throwable {
        // /sources/data/tinyG.txt
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

        GraphDFSPaths ps0 = new GraphDFSPaths(0, G);
        Assert.assertTrue(ps0.hasPathTo(0));
        Assert.assertTrue(ps0.hasPathTo(1));
        Assert.assertTrue(ps0.hasPathTo(2));
        Assert.assertTrue(ps0.hasPathTo(3));
        Assert.assertTrue(ps0.hasPathTo(4));
        Assert.assertTrue(ps0.hasPathTo(5));

        int l = 0;
        for (int x : ps0.pathTo(5)) { l++; }
        Assert.assertEquals(4, l);
        Assert.assertEquals("0-6-4-5", pathToString(5, ps0));

        Assert.assertTrue(ps0.hasPathTo(6));

        Assert.assertFalse(ps0.hasPathTo(7));
        for (int x : ps0.pathTo(7)) {}
        Assert.assertEquals("", pathToString(7, ps0));

        Assert.assertFalse(ps0.hasPathTo(9));

        GraphDFSPaths ps9 = new GraphDFSPaths(9, G);
        Assert.assertTrue(ps9.hasPathTo(9));
        Assert.assertTrue(ps9.hasPathTo(10));
        Assert.assertTrue(ps9.hasPathTo(11));
        Assert.assertTrue(ps9.hasPathTo(12));
        Assert.assertFalse(ps9.hasPathTo(0));
        Assert.assertFalse(ps9.hasPathTo(7));

        System.out.println("OK");
    }
}
