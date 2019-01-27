/*
public class public class Stack<Item> implements Iterable<Item> {
    public Stack();
    public void push(Item item);
}
*/

public class DigraphCycle {
    // SOLUTION_BEGIN
    private boolean[] marked;
    private int[] edgeTo;
    private boolean[] onStack;
    private Stack<Integer> cycle;
    // SOLUTION_END

    public DigraphCycle(Digraph G) {
        // SOLUTION_BEGIN
        marked = new boolean[G.V()];
        edgeTo = new int[G.V()];
        onStack = new boolean[G.V()];
        for (int v = 0; v < G.V(); v++) {
            if (!marked[v]) {
                dfs(G, v);
            }
        }
        // SOLUTION_END
    }

    // SOLUTION_BEGIN
    private void dfs(Digraph G, int v) {
        marked[v] = true;
        onStack[v] = true;
        for (int w : G.adj(v)) {
            // short circuit if cycle already found
            if (hasCycle()) return;
            if (!marked[w]) {
                edgeTo[w] = v;
                dfs(G, w);
            } else if (onStack[w]) {
                cycle = new Stack<>();
                for (int x = v; x != w; x = edgeTo[x]) {
                    cycle.push(x);
                }
                cycle.push(w);
                cycle.push(v);
            }
        }
        onStack[v] = false;
    }
    // SOLUTION_END

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

    public boolean checkCycle(Iterable<Integer> cycle) {
        // SOLUTION_BEGIN
        int first = -1, last = -1, len = 0;
        for (int v : cycle) {
            if (first == -1) first = v;
            last = v;
            len++;
        }
        return first == last && len > 0;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        // /data/digraph3.txt
        // no cycles
        Digraph G3 = new Digraph(6);
        G3.addEdge(0, 2);
        G3.addEdge(1, 2);
        G3.addEdge(2, 3);
        G3.addEdge(3, 4);
        G3.addEdge(3, 5);
        DigraphCycle c3 = new DigraphCycle(G3);
        Assert.assertFalse(c3.hasCycle());

        // /data/digraph4.txt
        // no cycles
        Digraph G4 = new Digraph(7);
        G4.addEdge(0, 1);
        G4.addEdge(0, 2);
        G4.addEdge(0, 5);
        G4.addEdge(0, 6);
        G4.addEdge(1, 3);
        G4.addEdge(2, 3);
        G4.addEdge(2, 4);
        G4.addEdge(4, 5);
        G4.addEdge(4, 6);
        DigraphCycle c4 = new DigraphCycle(G4);
        Assert.assertFalse(c4.hasCycle());

        // /data/digraph4-cycle.txt
        // one cycle
        Digraph G4C = new Digraph(7);
        G4C.addEdge(0, 1);
        G4C.addEdge(0, 2);
        G4C.addEdge(0, 5);
        G4C.addEdge(1, 3);
        G4C.addEdge(2, 3);
        G4C.addEdge(2, 4);
        G4C.addEdge(4, 5);
        G4C.addEdge(4, 6);
        G4C.addEdge(6, 0);
        DigraphCycle c4c = new DigraphCycle(G4C);
        Assert.assertTrue(c4c.hasCycle());
        Assert.assertEquals("6-0-2-4-6", GraphUtil.pathToString(c4c.cycle));
        Assert.assertTrue(c4c.checkCycle(c4c.cycle()));

        System.out.println("OK");
    }
}
