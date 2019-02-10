import java.util.NoSuchElementException;

/*
public class BinTree.Node<T> {
    public T item;
    public Node<T> left;
    public Node<T> right;
}
*/

public class BinSearchTreeDelete {
    public static <K extends Comparable<K>> BinTree.Node<K> delete(K key, BinTree.Node<K> n) {
        // BEGIN_SOLUTION
        if (n == null) return n;
        int cmp = key.compareTo(n.item);
        if      (cmp < 0) n.left  = delete(key, n.left);
        else if (cmp > 0) n.right = delete(key, n.right);
        else {
            if (n.left == null)  return n.right;
            if (n.right == null) return n.left;
            BinTree.Node<K> t = n;
            n = min(t.right);              // n = max(t.left);
            n.right = deleteMin(t.right);  // n.left = deleteMax(t.left)
            n.left = t.left;               // n.right = t.right;
        }
        return n;
        // END_SOLUTION
    }

    public static <K extends Comparable<K>> BinTree.Node<K> min(BinTree.Node<K> n) {
        if (n == null) throw new NoSuchElementException();
        if (n.left == null)
            return n;
        return min(n.left);
    }

    public static <K extends Comparable<K>> BinTree.Node<K> max(BinTree.Node<K> n) {
        if (n == null) throw new NoSuchElementException();
        if (n.right == null)
            return n;
        return max(n.right);
    }

    public static <K extends Comparable<K>> BinTree.Node<K> deleteMin(BinTree.Node<K> n) {
        if (n == null) throw new NoSuchElementException();
        if (n.left == null)
            return n.right;
        n.left = deleteMin(n.left);
        return n;
    }

    public static <K extends Comparable<K>> BinTree.Node<K> deleteMax(BinTree.Node<K> n) {
        if (n == null) throw new NoSuchElementException();
        if (n.right == null)
            return n.left;
        n.right = deleteMax(n.right);
        return n;
    }

    @SuppressWarnings("unchecked")
    public static void main(String[] args) throws Throwable {
        Assert.assertNull(delete(1, null));

        BinTree.Node<Integer> tree;
        tree = BinTree.fromIntArray(new int[] {1,2,3,4,5,6,7});
        tree = delete(1, tree);
        Assert.assertArrayEquals(new int[] {2,3,4,5,6,7}, BinTree.toIntArray(tree));
        tree = delete(7, tree);
        Assert.assertArrayEquals(new int[] {2,3,4,5,6}, BinTree.toIntArray(tree));
        tree = delete(4, tree);
        Assert.assertArrayEquals(new int[] {2,3,5,6}, BinTree.toIntArray(tree));

        System.out.println("OK");
    }
}
