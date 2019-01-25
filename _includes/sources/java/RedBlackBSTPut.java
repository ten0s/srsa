/*
public class BinTree {
    public static final boolean RED   = true;
    public static final boolean BLACK = false;

    public static class Node<T> {
        public T item;
        public Node<T> left;
        public Node<T> right;
        public int size;
        boolean color;
        public Node(T item, boolean color);
        public int size(Node<T> node);
    }
}

public class Entry<K, V> {
    public K key;
    public V val;
    public Entry(K key, V val);
}
*/

public class RedBlackBSTPut {
    public static <K extends Comparable<K>, V> BinTree.Node<Entry<K, V>> put(
        K key, V val, BinTree.Node<Entry<K, V>> node
    ) {
        // SOLUTION_BEGIN
        if (node == null) return new BinTree.Node<>(new Entry<>(key, val), BinTree.RED);
        int cmp = key.compareTo(node.item.key);
        if      (cmp < 0) node.left  = put(key, val, node.left);
        else if (cmp > 0) node.right = put(key, val, node.right);
        else              node.item.val = val;

        if (!isRed(node.left) && isRed(node.right))     node = rotateLeft(node);
        if ( isRed(node.left) && isRed(node.left.left)) node = rotateRight(node);
        if ( isRed(node.left) && isRed(node.right))     flipColors(node);

        node.size = 1 + BinTree.size(node.left) + BinTree.size(node.right);
        return node;
        // SOLUTION_END
    }

    private static boolean isRed(BinTree.Node<?> x) {
        // SOLUTION_BEGIN
        // null links are black
        if (x == null) return false;
        return x.color == BinTree.RED;
        // SOLUTION_END
    }

    private static <K, V> BinTree.Node<Entry<K, V>> rotateLeft(BinTree.Node<Entry<K, V>> h) {
        // SOLUTION_BEGIN
        assert isRed(h.right);
        BinTree.Node<Entry<K, V>> x = h.right;
        h.right = x.left;
        x.left = h;
        x.color = h.color;
        h.color = BinTree.RED;
        x.size = h.size;
        h.size = 1 + BinTree.size(h.left) + BinTree.size(h.right);
        return x;
        // SOLUTION_END
    }

    private static <K, V> BinTree.Node<Entry<K, V>> rotateRight(BinTree.Node<Entry<K, V>> h) {
        // SOLUTION_BEGIN
        assert isRed(h.left);
        BinTree.Node<Entry<K, V>> x = h.left;
        h.left = x.right;
        x.right = h;
        x.color = h.color;
        h.color = BinTree.RED;
        x.size = h.size;
        h.size = 1 + BinTree.size(h.left) + BinTree.size(h.right);
        return x;
        // SOLUTION_END
    }

    private static void flipColors(BinTree.Node<?> h) {
        // SOLUTION_BEGIN
        assert !isRed(h);
        assert isRed(h.left);
        assert isRed(h.right);
        h.color = !h.color;
        h.left.color = !h.left.color;
        h.right.color = !h.right.color;
        // SOLUTION_END
    }

    @SuppressWarnings("unchecked")
    public static void main(String[] args) throws Throwable {
        Entry[] arr = new Entry[] {
                new Entry<>(1, "one"),
                new Entry<>(2, "two"),
                new Entry<>(3, "three"),
                new Entry<>(4, "four"),
                new Entry<>(5, "five"),
                new Entry<>(6, "six"),
                new Entry<>(7, "seven"),
        };

        BinTree.Node<Entry<Integer, String>> tree = null;
        tree = put(1, "one", tree);
        tree = put(2, "two", tree);
        tree = put(3, "three", tree);
        tree = put(4, "four", tree);
        tree = put(5, "five", tree);
        tree = put(6, "six", tree);
        tree = put(7, "seven", tree);
        Assert.assertArrayEquals(arr, BinTree.toArray(tree));
        Assert.assertEquals(7, BinTree.size(tree));
        Assert.assertEquals(3, BinTree.height(tree));

        System.out.println("OK");
    }
}
