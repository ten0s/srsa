public class GraphCycle {
    // SOLUTION_BEGIN
    private boolean[] marked;
    private int[] edgeTo;
    private Stack<Integer> cycle;
    // SOLUTION_END

    public GraphCycle(Graph G) {
        // SOLUTION_BEGIN
        if (hasSelfLoop(G)) return;
        if (hasParallelEdges(G)) return;
        marked = new boolean[G.V()];
        edgeTo = new int[G.V()];
        for (int v = 0; v < G.V(); v++) {
            if (!marked[v]) {
                dfs(G, -1, v);
            }
        }
        // SOLUTION_END
    }

    private boolean hasSelfLoop(Graph G) {
        // SOLUTION_BEGIN
        for (int v = 0; v < G.V(); v++) {
            for (int w : G.adj(v)) {
                if (v == w) {
                    cycle = new Stack<>();
                    cycle.push(v);
                    cycle.push(w);
                    return true;
                }
            }
        }
        return false;
        // SOLUTION_END
    }

    private boolean hasParallelEdges(Graph G) {
        // SOLUTION_BEGIN
        marked = new boolean[G.V()];
        for (int v = 0; v < G.V(); v++) {
            for (int w : G.adj(v)) {
                if (marked[w]) {
                    cycle = new Stack<>();
                    cycle.push(v);
                    cycle.push(w);
                    cycle.push(v);
                    return true;
                }
                marked[w] = true;
            }
            for (int w : G.adj(v)) {
                marked[w] = false;
            }
        }
        return false;
        // SOLUTION_END
    }

    private void dfs(Graph G, int u, int v) {
        // SOLUTION_BEGIN
        marked[v] = true;
        for (int w : G.adj(v)) {
            // short circuit if cycle already found
            if (cycle != null) return;
            if (!marked[w]) {
                edgeTo[w] = v;
                dfs(G, v, w);
            } else {
                // if v's child is marked and is NOT v's parent
                if (w != u) {
                    cycle = new Stack<>();
                    for (int x = v; x != w; x = edgeTo[x]) {
                        cycle.push(x);
                    }
                    cycle.push(w);
                    cycle.push(v);
                }
            }
        }
        // SOLUTION_END
    }

    public boolean hasCycle() {
        // SOLUTION_BEGIN
        return cycle != null;
        // SOLUTION_END
    }

    public Iterable<Integer> cycle() {
        // SOLUTION_BEGIN
        return cycle;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        // /data/graph2.txt
        // self-loop
        Graph G2 = new Graph(3);
        G2.addEdge(0, 0);
        G2.addEdge(0, 1);
        G2.addEdge(0, 2);
        G2.addEdge(1, 2);
        GraphCycle c2 = new GraphCycle(G2);
        Assert.assertTrue(c2.hasCycle());
        Assert.assertEquals("0-0", GraphUtil.pathToString(c2.cycle));

        // /data/graph5.txt
        // parallel edge
        Graph G5 = new Graph(6);
        G5.addEdge(0, 2);
        G5.addEdge(1, 2);
        G5.addEdge(2, 3);
        G5.addEdge(3, 2);
        G5.addEdge(3, 4);
        G5.addEdge(3, 5);
        GraphCycle c5 = new GraphCycle(G5);
        Assert.assertTrue(c5.hasCycle());
        Assert.assertEquals("2-3-2", GraphUtil.pathToString(c5.cycle));

        // /data/graph3.txt
        // no cycles
        Graph G3 = new Graph(6);
        G3.addEdge(0, 2);
        G3.addEdge(1, 2);
        G3.addEdge(2, 3);
        G3.addEdge(3, 4);
        G3.addEdge(3, 5);
        GraphCycle c3 = new GraphCycle(G3);
        Assert.assertFalse(c3.hasCycle());

        // /data/graph4.txt
        // some cycles
        Graph G4 = new Graph(7);
        G4.addEdge(0, 1);
        G4.addEdge(0, 2);
        G4.addEdge(0, 5);
        G4.addEdge(0, 6);
        G4.addEdge(1, 3);
        G4.addEdge(2, 3);
        G4.addEdge(2, 4);
        G4.addEdge(4, 5);
        G4.addEdge(4, 6);
        GraphCycle c4 = new GraphCycle(G4);
        Assert.assertTrue(c4.hasCycle());
        // ["0-1-3-2-0", "5-0-6-4-5", ...]
        Assert.assertEquals("5-0-6-4-5", GraphUtil.pathToString(c4.cycle));

        System.out.println("OK");
    }
}
