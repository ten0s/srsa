import java.util.function.BiFunction;

/*
public class BinTree.Node<T> {
    public T item;
    public Node<T> left;
    public Node<T> right;
}
*/

public class BinTreeTraversal {
    public static <T, R> R preOrder(BiFunction<T, R, R> fun, R init, BinTree.Node<T> tree) {
        //+BEGIN_SOLUTION
        if (tree == null) return init;
        R n = fun.apply(tree.item, init);
        R l = preOrder(fun, n, tree.left);
        R r = preOrder(fun, l, tree.right);
        return r;
        //+END_SOLUTION
    }

    public static <T, R> R inOrder(BiFunction<T, R, R> fun, R init, BinTree.Node<T> tree) {
        //+BEGIN_SOLUTION
        if (tree == null) return init;
        R l = inOrder(fun, init, tree.left);
        R n = fun.apply(tree.item, l);
        R r = inOrder(fun, n, tree.right);
        return r;
        //+END_SOLUTION
    }

    public static <T, R> R postOrder(BiFunction<T, R, R> fun, R init, BinTree.Node<T> tree) {
        //+BEGIN_SOLUTION
        if (tree == null) return init;
        R l = postOrder(fun, init, tree.left);
        R r = postOrder(fun, l, tree.right);
        R n = fun.apply(tree.item, r);
        return n;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        BinTree.Node<Integer> tree = BinTree.fromIntArray(new int[] {1, 2, 3, 4, 5, 6 ,7});
        Assert.assertEquals(28,  preOrder((x, acc) -> x + acc, 0, tree));
        Assert.assertEquals(28,   inOrder((x, acc) -> x + acc, 0, tree));
        Assert.assertEquals(28, postOrder((x, acc) -> x + acc, 0, tree));

        BinTree.Node<String> tree2 = BinTree.fromArray(new String[] {"1", "2", "3", "4", "5", "6", "7"});
        Assert.assertEquals("4213657",  preOrder((x, acc) -> acc + x, "", tree2));
        Assert.assertEquals("1234567",   inOrder((x, acc) -> acc + x, "", tree2));
        Assert.assertEquals("1325764", postOrder((x, acc) -> acc + x, "", tree2));

        System.out.println("OK");
    }
    //+END_FOLD }
}
