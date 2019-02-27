public class GraphDegree {
    public static int degree(Graph G, int v) {
        //+BEGIN_SOLUTION
        int d = 0;
        for (int w : G.adj(v)) d++;
        return d;
        //+END_SOLUTION
    }

    public static int maxDegree(Graph G) {
        //+BEGIN_SOLUTION
        int max = 0;
        for (int v = 0; v < G.V(); v++) {
            max = Math.max(max, degree(G, v));
        }
        return max;
        //+END_SOLUTION
    }

    public static double avgDegree(Graph G) {
        //+BEGIN_SOLUTION
        // Each edge gives two degrees distributed among all vertices
        return 2.0 * G.E() / G.V();
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/graph1.txt
        Graph G = new Graph(10);
        G.addEdge(0, 1);
        G.addEdge(0, 7);
        G.addEdge(1, 2);
        G.addEdge(1, 3);
        G.addEdge(1, 7);
        G.addEdge(1, 8);
        G.addEdge(2, 3);
        G.addEdge(3, 4);
        G.addEdge(3, 5);
        G.addEdge(3, 8);
        G.addEdge(4, 5);
        G.addEdge(5, 6);
        G.addEdge(5, 7);
        G.addEdge(5, 8);
        G.addEdge(6, 7);
        G.addEdge(7, 8);
        //System.out.println(G);
        //System.out.println(G.toDot());

        Assert.assertEquals(2, degree(G, 0));
        Assert.assertEquals(5, degree(G, 1));
        Assert.assertEquals(4, degree(G, 8));
        Assert.assertEquals(5, maxDegree(G));
        Assert.assertEquals(3.2, avgDegree(G), 0.1);

        System.out.println("OK");
    }
    //+END_FOLD }
}
