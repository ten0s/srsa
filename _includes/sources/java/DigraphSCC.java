public class DigraphSCC {
    //+BEGIN_SOLUTION
    private boolean[] marked;
    private int[] id;
    private int[] size;
    private int count;
    //+END_SOLUTION

    public DigraphSCC(Digraph G) {
        //+BEGIN_SOLUTION
        marked = new boolean[G.V()];
        id = new int[G.V()];
        size = new int[G.V()];
        DigraphOrders orders = new DigraphOrders(reverse(G));
        for (int v : orders.reversedPostOrder()) {
            if (!marked[v]) {
                dfs(G, v);
                count++;
            }
        }
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private void dfs(Digraph G, int v) {
        marked[v] = true;
        id[v] = count;
        size[count]++;
        for (int w : G.adj(v)) {
            if (!marked[w]) {
                dfs(G, w);
            }
        }
    }

    private Digraph reverse(Digraph G) {
        Digraph R = new Digraph(G.V());
        for (int v = 0; v < G.V(); v++) {
            for (int w : G.adj(v)) {
                R.addEdge(w, v);
            }
        }
        return R;
    }
    //+END_SOLUTION

    public boolean stronglyConnected(int v, int w) {
        //+BEGIN_SOLUTION
        return id(v) == id(w);
        //+END_SOLUTION
    }

    public int id(int v) {
        //+BEGIN_SOLUTION
        return id[v];
        //+END_SOLUTION
    }

    // size of v's component
    private int size(int v) {
        //+BEGIN_SOLUTION
        return size[id[v]];
        //+END_SOLUTION
    }

    public int count() {
        //+BEGIN_SOLUTION
        return count;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
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

        DigraphSCC scc = new DigraphSCC(G);
        Assert.assertEquals(5, scc.count());

        // 0
        Assert.assertEquals(0, scc.id(1));
        Assert.assertEquals(1, scc.size(1));
        Assert.assertFalse(scc.stronglyConnected(0, 1));
        Assert.assertFalse(scc.stronglyConnected(0, 6));
        Assert.assertFalse(scc.stronglyConnected(0, 7));
        Assert.assertFalse(scc.stronglyConnected(0, 9));

        // 1
        Assert.assertEquals(1, scc.id(0));
        Assert.assertEquals(5, scc.size(0));
        Assert.assertTrue(scc.stronglyConnected(0, 2));
        Assert.assertTrue(scc.stronglyConnected(0, 3));
        Assert.assertTrue(scc.stronglyConnected(0, 4));
        Assert.assertTrue(scc.stronglyConnected(0, 5));

        // 2
        Assert.assertEquals(2, scc.id(9));
        Assert.assertEquals(4, scc.size(9));
        Assert.assertTrue(scc.stronglyConnected(9, 10));
        Assert.assertTrue(scc.stronglyConnected(9, 11));
        Assert.assertTrue(scc.stronglyConnected(9, 12));

        // 3
        Assert.assertEquals(3, scc.id(6));
        Assert.assertEquals(2, scc.size(6));
        Assert.assertTrue(scc.stronglyConnected(6, 8));

        // 4
        Assert.assertEquals(4, scc.id(7));
        Assert.assertEquals(1, scc.size(7));

        System.out.println("OK");
    }
    //+END_FOLD }
}

// Refs
/*+BEGIN_FOLD
public class Digraph {
    public Iterable<Integer> adj(int v);
}

public class DigraphOrders {
    public DigraphOrders(Digraph G);
    public Iterable<Integer> preOrder();
    public Iterable<Integer> postOrder();
    public Iterable<Integer> reversedPostOrder();
}
+END_FOLD*/
