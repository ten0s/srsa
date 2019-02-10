import java.util.NoSuchElementException;

/*
public class BinTree.Node<T> {
    public T item;
    public Node<T> left;
    public Node<T> right;
}
*/

public class BinSearchTreeDeleteMinMax {
    public static <K extends Comparable<K>> BinTree.Node<K> deleteMin(BinTree.Node<K> n) {
        //+BEGIN_SOLUTION
        if (n == null) {
            throw new NoSuchElementException();
        }
        if (n.left != null) {
            n.left = deleteMin(n.left);
        } else {
            n = n.right;
        }
        return n;
        //+END_SOLUTION
    }

    public static <K extends Comparable<K>> BinTree.Node<K> deleteMax(BinTree.Node<K> n) {
        //+BEGIN_SOLUTION
        if (n == null) {
            throw new NoSuchElementException();
        }
        if (n.right != null) {
            n.right = deleteMax(n.right);
        } else {
            n = n.left;
        }
        return n;
        //+END_SOLUTION
    }

    @SuppressWarnings("unchecked")
    public static void main(String[] args) throws Throwable {
        BinTree.Node<Integer> tree;

        try { deleteMin(null); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        tree = BinTree.fromIntArray(new int[] {1,2,3,4,5,6,7});
        tree = deleteMin(tree);
        Assert.assertArrayEquals(new int[] {2,3,4,5,6,7}, BinTree.toIntArray(tree));
        tree = deleteMin(tree);
        Assert.assertArrayEquals(new int[] {3,4,5,6,7}, BinTree.toIntArray(tree));

        try { deleteMax(null); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        tree = BinTree.fromIntArray(new int[] {1,2,3,4,5,6,7});
        tree = deleteMax(tree);
        Assert.assertArrayEquals(new int[] {1,2,3,4,5,6}, BinTree.toIntArray(tree));
        tree = deleteMax(tree);
        Assert.assertArrayEquals(new int[] {1,2,3,4,5}, BinTree.toIntArray(tree));

        System.out.println("OK");
    }
}
