/*
public class public class Queue<Item> implements Iterable<Item> {
    public Queue();
    public void enqueue(Item item);
}

public class public class Stack<Item> implements Iterable<Item> {
    public Stack();
    public void push(Item item);
}
*/

public class DigraphOrders {
    // SOLUTION_BEGIN
    private boolean[] marked;
    private Queue<Integer> pre;
    private Queue<Integer> post;
    private Stack<Integer> reversedPost;
    // SOLUTION_END

    public DigraphOrders(Digraph G) {
        // SOLUTION_BEGIN
        marked = new boolean[G.V()];
        pre = new Queue<>();
        post = new Queue<>();
        reversedPost = new Stack<>();
        for (int v = 0; v < G.V(); v++) {
            if (!marked[v]) {
                dfs(G, v);
            }
        }
        // SOLUTION_END
    }

    // SOLUTION_BEGIN
    private void dfs(Digraph G, int v) {
        pre.enqueue(v);
        marked[v] = true;
        for (int w : G.adj(v)) {
            if (!marked[w]) {
                dfs(G, w);
            }
        }
        post.enqueue(v);
        reversedPost.push(v);
    }
    // SOLUTION_END

    public Iterable<Integer> preOrder() {
        // SOLUTION_BEGIN
        return pre;
        // SOLUTION_END
    }

    public Iterable<Integer> postOrder() {
        // SOLUTION_BEGIN
        return post;
        // SOLUTION_END
    }

    public Iterable<Integer> reversedPostOrder() {
        // SOLUTION_BEGIN
        return reversedPost;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        // /data/digraph4.txt
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
        DigraphOrders o4 = new DigraphOrders(G4);
        // NB:
        // Digraph.addEdge(v, w) prepends vertices,
        // so Digraph.adj(v) returns them in reverse order
        Assert.assertEquals("0-6-5-2-4-3-1", GraphUtil.pathToString(o4.preOrder()));
        Assert.assertEquals("6-5-4-3-2-1-0", GraphUtil.pathToString(o4.postOrder()));
        Assert.assertEquals("0-1-2-3-4-5-6", GraphUtil.pathToString(o4.reversedPostOrder()));
        System.out.println("OK");
    }
}
