/*
public class BinTree.Node<T> {
    public T item;
    public Node<T> left;
    public Node<T> right;
}

public class Entry<K, V> {
    public K key;
    public V val;
    public Entry(K key, V val);
}
*/

public class BinSearchTreeGetPut {
    public static <K extends Comparable<K>, V> V get(
        K key, BinTree.Node<Entry<K, V>> node
    ) {
        // SOLUTION_BEGIN
        if (node == null) return null;
        int cmp = key.compareTo(node.item.key);
        if (cmp < 0) return get(key, node.left);
        if (cmp > 0) return get(key, node.right);
        return node.item.val;
        // SOLUTION_END
    }

    public static <K extends Comparable<K>, V> BinTree.Node<Entry<K, V>> put(
        K key, V val, BinTree.Node<Entry<K, V>> node
    ) {
        // SOLUTION_BEGIN
        if (node == null) return new BinTree.Node<>(new Entry<>(key, val));
        int cmp = key.compareTo(node.item.key);
        if      (cmp < 0) node.left  = put(key, val, node.left);
        else if (cmp > 0) node.right = put(key, val, node.right);
        else              node.item.val = val;
        return node;
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
                new Entry<>(7, "seven")
        };
        BinTree.Node<Entry<Integer, String>> tree = BinTree.fromArray(arr);

        // test get
        Assert.assertNull(get(1, null));
        Assert.assertNull(get(0, tree));
        Assert.assertEquals("one", get(1, tree));
        Assert.assertEquals("two", get(2, tree));
        Assert.assertEquals("four", get(4, tree));
        Assert.assertEquals("six", get(6, tree));
        Assert.assertEquals("seven", get(7, tree));
        Assert.assertNull(get(8, tree));

        // test put
        tree = put(4, "four", tree);
        tree = put(2, "two", tree);
        tree = put(6, "six", tree);
        tree = put(1, "one", tree);
        tree = put(3, "three", tree);
        tree = put(5, "five", tree);
        tree = put(7, "seven", tree);
        Assert.assertArrayEquals(arr, BinTree.toArray(tree));

        System.out.println("OK");
    }
}
