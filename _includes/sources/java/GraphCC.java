public class GraphCC {
    //+BEGIN_SOLUTION
    private boolean[] marked;
    private int[] id;
    private int[] size;
    private int count;
    //+END_SOLUTION

    public GraphCC(Graph G) {
        //+BEGIN_SOLUTION
        marked = new boolean[G.V()];
        id = new int[G.V()];
        size = new int[G.V()];
        for (int v = 0; v < G.V(); v++) {
            if (!marked[v]) {
                dfs(G, v);
                count++;
            }
        }
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private void dfs(Graph G, int v) {
        marked[v] = true;
        id[v] = count;
        size[count]++;
        for (int w : G.adj(v)) {
            if (!marked[w]) {
                dfs(G, w);
            }
        }
    }
    //+END_SOLUTION

    public boolean connected(int v, int w) {
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
        Assert.assertEquals(7, cc.size(0));
        for (int v = 1; v <= 6; v++) {
            Assert.assertTrue(cc.connected(0, v));
        }
        Assert.assertFalse(cc.connected(0, 7));
        Assert.assertFalse(cc.connected(0, 9));

        // 1
        Assert.assertEquals(1, cc.id(7));
        Assert.assertEquals(2, cc.size(7));
        Assert.assertTrue(cc.connected(7, 8));
        Assert.assertFalse(cc.connected(7, 9));

        // 2
        Assert.assertEquals(2, cc.id(9));
        Assert.assertEquals(4, cc.size(9));
        for (int v = 10; v <= 12; v++) {
            Assert.assertTrue(cc.connected(9, v));
        }

        System.out.println("OK");
    }
    //+END_FOLD }
}
