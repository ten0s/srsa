import java.util.NoSuchElementException;

/*
public class BinTree.Node<T> {
    public T item;
    public Node<T> left;
    public Node<T> right;
}
*/

public class BinSearchTreeMinMax {
    public static <K extends Comparable<K>> K min(BinTree.Node<K> node) {
        // SOLUTION_BEGIN
        if (node == null)
            throw new NoSuchElementException();
        if (node.left == null)
            return node.item;
        return min(node.left);
        // SOLUTION_END
    }

    public static <K extends Comparable<K>> K max(BinTree.Node<K> node) {
        // SOLUTION_BEGIN
        if (node == null)
            throw new NoSuchElementException();
        if (node.right == null)
            return node.item;
        return max(node.right);
        // SOLUTION_END
    }

    @SuppressWarnings("unchecked")
    public static void main(String[] args) throws Throwable {
        BinTree.Node<Integer> tree = BinTree.fromIntArray(new int[] {1,2,3,4,5,6,7});

        try { min(null); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        Assert.assertEquals(1, min(tree));

        try { max(null); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        Assert.assertEquals(7, max(tree));

        System.out.println("OK");
    }
}
