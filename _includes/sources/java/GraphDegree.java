/*
public class Graph {
    public int V()
    public int E();
    public Iterable<Integer> adj(int v);
}
*/

public class GraphDegree {
    public static int degree(int v, Graph G) {
        // SOLUTION_BEGIN
        int d = 0;
        for (int w : G.adj(v)) d++;
        return d;
        // SOLUTION_END
    }

    public static int maxDegree(Graph G) {
        // SOLUTION_BEGIN
        int max = 0;
        for (int v = 0; v < G.V(); v++) {
            max = Math.max(max, degree(v, G));
        }
        return max;
        // SOLUTION_END
    }

    public static double avgDegree(Graph G) {
        // SOLUTION_BEGIN
        // Each edge gives two degrees distributed among all vertices
        return 2.0 * G.E() / G.V();
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
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

        Assert.assertEquals(2, degree(0, G));
        Assert.assertEquals(5, degree(1, G));
        Assert.assertEquals(4, degree(8, G));
        Assert.assertEquals(5, maxDegree(G));
        Assert.assertEquals(3.2, avgDegree(G), 0.1);

        System.out.println("OK");
    }
}
