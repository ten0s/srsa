/*
public class BinTree.Node<?> {
    public Node<?> left;
    public Node<?> right;
}
*/

public class BinTreeSize {
    public static int size(BinTree.Node<?> root) {
        // BEGIN_SOLUTION
        if (root == null) {
            return 0;
        }
        return 1 + size(root.left) + size(root.right);
        // END_SOLUTION
    }

    public static void main(String[] args) throws Throwable {
        BinTree.Node<Integer> tree = null;
        Assert.assertEquals(0, size(tree));

        tree = new BinTree.Node<>(3);
        Assert.assertEquals(1, size(tree));

        tree.left = new BinTree.Node<>(2);
        tree.right = new BinTree.Node<>(4);
        Assert.assertEquals(3, size(tree));

        tree.left.left = new BinTree.Node<>(1);
        Assert.assertEquals(4, size(tree));

        System.out.println("OK");
    }
}
