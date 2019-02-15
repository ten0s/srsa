//+BEGIN_SOLUTION
import java.util.Deque;
import java.util.ArrayDeque;
//+END_SOLUTION

public class DigraphOrders {
    //+BEGIN_SOLUTION
    private boolean[] marked;
    private Deque<Integer> pre;
    private Deque<Integer> post;
    private Deque<Integer> revPost;
    //+END_SOLUTION

    public DigraphOrders(Digraph G) {
        //+BEGIN_SOLUTION
        marked = new boolean[G.V()];
        pre = new ArrayDeque<>();
        post = new ArrayDeque<>();
        revPost = new ArrayDeque<>();
        for (int v = 0; v < G.V(); v++) {
            if (!marked[v]) {
                dfs(G, v);
            }
        }
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private void dfs(Digraph G, int v) {
        pre.add(v);
        marked[v] = true;
        for (int w : G.adj(v)) {
            if (!marked[w]) {
                dfs(G, w);
            }
        }
        post.add(v);
        revPost.push(v);
    }
    //+END_SOLUTION

    public Iterable<Integer> preOrder() {
        //+BEGIN_SOLUTION
        return pre;
        //+END_SOLUTION
    }

    public Iterable<Integer> postOrder() {
        //+BEGIN_SOLUTION
        return post;
        //+END_SOLUTION
    }

    public Iterable<Integer> reversedPostOrder() {
        //+BEGIN_SOLUTION
        return revPost;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
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
    //+END_FOLD }
}
