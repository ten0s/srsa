import java.util.NoSuchElementException;

/*
public class BinTree.Node<T> {
    public T item;
    public Node<T> left;
    public Node<T> right;
    public static int size(Node<T> node);
}
*/

public class BinSearchTreeRankSelect {
    public static <K extends Comparable<K>> int rank(K key, BinTree.Node<K> n) {
        // BEGIN_SOLUTION
        if (n == null) return 0;
        int cmp = key.compareTo(n.item);
        if      (cmp < 0) return rank(key, n.left);
        else if (cmp > 0) return 1 + BinTree.size(n.left) + rank(key, n.right);
        else              return BinTree.size(n.left);
        // END_SOLUTION
    }

    public static <K extends Comparable<K>> BinTree.Node<K> select(int k, BinTree.Node<K> n) {
        // BEGIN_SOLUTION
        if (n == null) return null;
        int r = BinTree.size(n.left);
        if      (r > k) return select(k, n.left);
        else if (r < k) return select(k-r-1, n.right);
        else            return n;
        // END_SOLUTION
    }

    @SuppressWarnings("unchecked")
    public static void main(String[] args) throws Throwable {
        int[] keys = new int[] {1,3,5,6,7,9};
        BinTree.Node<Integer> tree = BinTree.fromIntArray(keys);
        //BinTree.println(tree);

        // Property #1
        // for i in [0..size(keys)-1] { i == rank(select(i)) }
        for (int i = 0; i < keys.length; i++) {
            Assert.assertEquals(i, rank(select(i, tree).item, tree));
        }

        // Property #2
        // for all key in keys { key == select(rank(key)) }
        for (int key : keys) {
            Assert.assertEquals(key, select(rank(key, tree), tree).item);
        }

        System.out.println("OK");
    }
}
