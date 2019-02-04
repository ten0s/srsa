public class DigraphTransitiveClosure {
    // SOLUTION_BEGIN
    private DigraphDFS[] all;
    // SOLUTION_END

    public DigraphTransitiveClosure(Digraph G) {
        // SOLUTION_BEGIN
        all = new DigraphDFS[G.V()];
        for (int v = 0; v < G.V(); v++) {
            all[v] = new DigraphDFS(G, v);
        }
        // SOLUTION_END
    }

    public boolean reachable(int v, int w) {
        // SOLUTION_BEGIN
        return all[v].hasPathTo(w);
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        // /data/tinyDG.txt
        Digraph G = new Digraph(13);
        G.addEdge(4, 2);
        G.addEdge(2, 3);
        G.addEdge(3, 2);
        G.addEdge(6, 0);
        G.addEdge(0, 1);
        G.addEdge(2, 0);
        G.addEdge(11, 12);
        G.addEdge(12, 9);
        G.addEdge(9, 10);
        G.addEdge(9, 11);
        G.addEdge(7, 9);
        G.addEdge(10, 12);
        G.addEdge(11, 4);
        G.addEdge(4, 3);
        G.addEdge(3, 5);
        G.addEdge(6, 8);
        G.addEdge(8, 6);
        G.addEdge(5, 4);
        G.addEdge(0, 5);
        G.addEdge(6, 4);
        G.addEdge(6, 9);
        G.addEdge(7, 6);

        DigraphTransitiveClosure tc = new DigraphTransitiveClosure(G);
        Assert.assertTrue(tc.reachable(0, 1));
        Assert.assertFalse(tc.reachable(1, 0));
        Assert.assertTrue(tc.reachable(0, 2));
        Assert.assertTrue(tc.reachable(0, 3));
        Assert.assertTrue(tc.reachable(0, 4));
        Assert.assertTrue(tc.reachable(0, 5));

        Assert.assertFalse(tc.reachable(0, 6));
        Assert.assertTrue(tc.reachable(6, 0));

        Assert.assertFalse(tc.reachable(0, 7));
        Assert.assertTrue(tc.reachable(7, 0));

        Assert.assertFalse(tc.reachable(0, 9));
        Assert.assertTrue(tc.reachable(9, 0));

        System.out.println("OK");
    }
}

/*
public class DigraphDFS {
    public DigraphDFS(Digraph G, int s);
    public boolean hasPathTo(int v);
}
*/
