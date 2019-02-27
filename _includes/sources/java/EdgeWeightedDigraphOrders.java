//+BEGIN_SOLUTION
import java.util.Deque;
import java.util.ArrayDeque;
//+END_SOLUTION

public class EdgeWeightedDigraphOrders {
    //+BEGIN_SOLUTION
    private boolean[] marked;
    private Deque<Integer> pre;
    private Deque<Integer> post;
    private Deque<Integer> revPost;
    //+END_SOLUTION

    public EdgeWeightedDigraphOrders(EdgeWeightedDigraph G) {
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
    private void dfs(EdgeWeightedDigraph G, int v) {
        pre.add(v);
        marked[v] = true;
        for (DirectedEdge e : G.adj(v)) {
            int w = e.to();
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
        // data/ewdigraph4.txt
        // no cycles
        EdgeWeightedDigraph G = new EdgeWeightedDigraph(7);
        G.addEdge(new DirectedEdge(0, 1, 0.35));
        G.addEdge(new DirectedEdge(0, 2, 0.37));
        G.addEdge(new DirectedEdge(0, 5, 0.28));
        G.addEdge(new DirectedEdge(0, 6, 0.16));
        G.addEdge(new DirectedEdge(1, 3, 0.32));
        G.addEdge(new DirectedEdge(2, 3, 0.38));
        G.addEdge(new DirectedEdge(2, 4, 0.17));
        G.addEdge(new DirectedEdge(4, 5, 0.19));
        G.addEdge(new DirectedEdge(4, 6, 0.26));
        //System.out.println(G);
        //System.out.println(G.toDot());

        EdgeWeightedDigraphOrders o = new EdgeWeightedDigraphOrders(G);
        Assert.assertEquals("0-1-3-2-4-5-6", GraphUtil.pathToString(o.preOrder()));
        Assert.assertEquals("3-1-5-6-4-2-0", GraphUtil.pathToString(o.postOrder()));
        Assert.assertEquals("0-2-4-6-5-1-3", GraphUtil.pathToString(o.reversedPostOrder()));
        System.out.println("OK");
    }
    //+END_FOLD }
}
