import java.util.function.BiFunction;
//+BEGIN_SOLUTION
import java.util.Deque;
import java.util.ArrayDeque;
//+END_SOLUTION

/*
public class BinTree.Node<T> {
    public T item;
    public Node<T> left;
    public Node<T> right;
}
*/

public class BinTreeTraversal {
    public static <T, R> R preOrder(BiFunction<T, R, R> fun, R init, BinTree.Node<T> node) {
        //+BEGIN_SOLUTION
        if (node == null) return init;
        R n = fun.apply(node.item, init);
        R l = preOrder(fun, n, node.left);
        R r = preOrder(fun, l, node.right);
        return r;
        //+END_SOLUTION
    }

    public static <T, R> R inOrder(BiFunction<T, R, R> fun, R init, BinTree.Node<T> node) {
        //+BEGIN_SOLUTION
        if (node == null) return init;
        R l = inOrder(fun, init, node.left);
        R n = fun.apply(node.item, l);
        R r = inOrder(fun, n, node.right);
        return r;
        //+END_SOLUTION
    }

    public static <T, R> R postOrder(BiFunction<T, R, R> fun, R init, BinTree.Node<T> node) {
        //+BEGIN_SOLUTION
        if (node == null) return init;
        R l = postOrder(fun, init, node.left);
        R r = postOrder(fun, l, node.right);
        R n = fun.apply(node.item, r);
        return n;
        //+END_SOLUTION
    }

    public static <T, R> R levelOrder(BiFunction<T, R, R> fun, R init, BinTree.Node<T> node) {
        //+BEGIN_SOLUTION
        if (node == null) return init;
        R acc = init;
        Deque<BinTree.Node<T>> queue = new ArrayDeque<>();
        queue.add(node);
        while (!queue.isEmpty()) {
            BinTree.Node<T> x = queue.remove();
            acc = fun.apply(x.item, acc);
            if (x.left  != null) queue.add(x.left);
            if (x.right != null) queue.add(x.right);
        }
        return acc;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        BinTree.Node<Integer> tree = BinTree.fromIntArray(new int[] {1, 2, 3, 4, 5, 6 ,7});
        Assert.assertEquals(28, (int)  preOrder((x, acc) -> x + acc, 0, tree));
        Assert.assertEquals(28, (int)   inOrder((x, acc) -> x + acc, 0, tree));
        Assert.assertEquals(28, (int) postOrder((x, acc) -> x + acc, 0, tree));
        Assert.assertEquals(28, (int)levelOrder((x, acc) -> x + acc, 0, tree));

        BinTree.Node<String> tree2 = BinTree.fromArray(new String[] {"1", "2", "3", "4", "5", "6", "7"});
        Assert.assertEquals("4213657",   preOrder((x, acc) -> acc + x, "", tree2));
        Assert.assertEquals("1234567",    inOrder((x, acc) -> acc + x, "", tree2));
        Assert.assertEquals("1325764",  postOrder((x, acc) -> acc + x, "", tree2));
        Assert.assertEquals("4261357", levelOrder((x, acc) -> acc + x, "", tree2));

        System.out.println("OK");
    }
    //+END_FOLD }
}
