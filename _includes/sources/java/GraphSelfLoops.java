public class GraphSelfLoops {
    public static int selfLoops(Graph G) {
        // BEGIN_SOLUTION
        int count = 0;
        for (int v = 0; v < G.V(); v++) {
            for (int w : G.adj(v)) {
                if (v == w) count++;
            }
        }
        // each edge added twice
        return count/2;
        // END_SOLUTION
    }

    public static void main(String[] args) throws Throwable {
        // /data/graph1.txt
        Graph G1 = new Graph(10);
        G1.addEdge(0, 1);
        G1.addEdge(0, 7);
        G1.addEdge(1, 2);
        G1.addEdge(1, 3);
        G1.addEdge(1, 7);
        G1.addEdge(1, 8);
        G1.addEdge(2, 3);
        G1.addEdge(3, 4);
        G1.addEdge(3, 5);
        G1.addEdge(3, 8);
        G1.addEdge(4, 5);
        G1.addEdge(5, 6);
        G1.addEdge(5, 7);
        G1.addEdge(5, 8);
        G1.addEdge(6, 7);
        G1.addEdge(7, 8);
        //System.out.println(G1);
        //System.out.println(G1.toDot());
        Assert.assertEquals(0, selfLoops(G1));

        // /data/graph2.txt
        Graph G2 = new Graph(3);
        G2.addEdge(0, 0);
        G2.addEdge(0, 1);
        G2.addEdge(0, 2);
        G2.addEdge(1, 2);
        //System.out.println(G2);
        //System.out.println(G2.toDot());
        Assert.assertEquals(1, selfLoops(G2));

        System.out.println("OK");
    }
}

/*
public class Graph {
    public int V()
    public int E();
    public Iterable<Integer> adj(int v);
}
*/
