import java.util.NoSuchElementException;

/*
public class BinTree.Node<T> {
    public T item;
    public Node<T> left;
    public Node<T> right;
}
*/

public class BinSearchTreeDeleteMinMax {
    public static <K extends Comparable<K>> BinTree.Node<K> deleteMin(BinTree.Node<K> node) {
        // SOLUTION_BEGIN
        if (node == null)
            throw new NoSuchElementException();
        if (node.left == null)
            return null;
        node.left = deleteMin(node.left);
        return node;
        // SOLUTION_END
    }

    public static <K extends Comparable<K>> BinTree.Node<K> deleteMax(BinTree.Node<K> node) {
        // SOLUTION_BEGIN
        if (node == null)
            throw new NoSuchElementException();
        if (node.right == null)
            return null;
        node.right = deleteMax(node.right);
        return node;
        // SOLUTION_END
    }

    @SuppressWarnings("unchecked")
    public static void main(String[] args) throws Throwable {

        try { deleteMin(null); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        Assert.assertArrayEquals(new int[] {2,3},
                                 BinTree.toIntArray(
                                     deleteMin(
                                         BinTree.fromIntArray(
                                             new int[] {1,2,3}))));

        try { deleteMax(null); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        Assert.assertArrayEquals(new int[] {1,2},
                                 BinTree.toIntArray(
                                     deleteMax(
                                         BinTree.fromIntArray(
                                             new int[] {1,2,3}))));

        System.out.println("OK");
    }
}
