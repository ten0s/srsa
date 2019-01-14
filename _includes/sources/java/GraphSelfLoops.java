/*
public class Graph {
    public int V()
    public int E();
    public Iterable<Integer> adj(int v);
}
*/

public class GraphSelfLoops {
    public static int selfLoops(Graph G) {
        // SOLUTION_BEGIN
        int count = 0;
        for (int v = 0; v < G.V(); v++) {
            for (int w : G.adj(v)) {
                if (v == w) count++;
            }
        }
        return count;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        // /sources/data/graph1.txt
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
        Assert.assertEquals(0, selfLoops(G));

        // /sources/data/graph2.txt
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
