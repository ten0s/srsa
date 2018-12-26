/*
public class BinTree.Node<?> {
    public Node<?> left;
    public Node<?> right;
}
*/

public class BinTreeHeight {
    public static int height(BinTree.Node<?> n) {
        // SOLUTION_BEGIN
        if (n == null) {
            return 0;
        }
        return 1 + Math.max(height(n.left), height(n.right));
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        BinTree.Node<Integer> tree = null;
        Assert.assertEquals(0, height(tree));

        tree = new BinTree.Node<>(3);
        Assert.assertEquals(1, height(tree));

        tree.left = new BinTree.Node<>(2);
        tree.right = new BinTree.Node<>(4);
        Assert.assertEquals(2, height(tree));

        tree.left.left = new BinTree.Node<>(1);
        Assert.assertEquals(3, height(tree));

        System.out.println("OK");
    }
}
