public class DigraphDFS {
    // BEGIN_SOLUTION
    private boolean[] marked;
    // END_SOLUTION

    public DigraphDFS(Digraph G, int s) {
        // BEGIN_SOLUTION
        marked = new boolean[G.V()];
        dfs(G, s);
        // END_SOLUTION
    }

    public DigraphDFS(Digraph G, Iterable<Integer> sources) {
        // BEGIN_SOLUTION
        marked = new boolean[G.V()];
        for (int s : sources) {
            if (!marked[s]) {
                dfs(G, s);
            }
        }
        // END_SOLUTION
    }

    // BEGIN_SOLUTION
    private void dfs(Digraph G, int v) {
        marked[v] = true;
        for (int w : G.adj(v)) {
            if (!marked[w]) {
                dfs(G, w);
            }
        }
    }
    // END_SOLUTION

    public boolean hasPathTo(int v) {
        // BEGIN_SOLUTION
        return marked[v];
        // END_SOLUTION
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

        DigraphDFS sp0 = new DigraphDFS(G, 0);
        Assert.assertTrue(sp0.hasPathTo(0));
        Assert.assertTrue(sp0.hasPathTo(1));
        Assert.assertTrue(sp0.hasPathTo(2));
        Assert.assertTrue(sp0.hasPathTo(3));
        Assert.assertTrue(sp0.hasPathTo(4));
        Assert.assertTrue(sp0.hasPathTo(5));
        Assert.assertFalse(sp0.hasPathTo(6));
        Assert.assertFalse(sp0.hasPathTo(7));
        Assert.assertFalse(sp0.hasPathTo(9));

        Bag<Integer> s69 = new Bag<>();
        s69.add(6); s69.add(9);
        DigraphDFS sp69 = new DigraphDFS(G, s69);
        Assert.assertTrue(sp69.hasPathTo(0));
        Assert.assertTrue(sp69.hasPathTo(8));
        Assert.assertFalse(sp69.hasPathTo(7));

        System.out.println("OK");
    }
}
