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

public class BinSearchTreeGet {
    public static <K extends Comparable<K>, V> V get(
        K key, BinTree.Node<Entry<K, V>> node
    ) {
        //+BEGIN_SOLUTION
        if (node == null) return null;
        int cmp = key.compareTo(node.item.key);
        if (cmp < 0) return get(key, node.left);
        if (cmp > 0) return get(key, node.right);
        return node.item.val;
        //+END_SOLUTION
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
        Assert.assertNull(get(1, null));
        Assert.assertNull(get(0, tree));
        Assert.assertEquals("one", get(1, tree));
        Assert.assertEquals("two", get(2, tree));
        Assert.assertEquals("four", get(4, tree));
        Assert.assertEquals("six", get(6, tree));
        Assert.assertEquals("seven", get(7, tree));
        Assert.assertNull(get(8, tree));

        System.out.println("OK");
    }
}
