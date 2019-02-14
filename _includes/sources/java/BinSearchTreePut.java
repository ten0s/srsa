/*
public class BinTree.Node<T> {
    public T item;
    public Node<T> left;
    public Node<T> right;
    public int size;
    public Node(T item);
    public int size(Node<T> node);
}

public class Entry<K, V> {
    public K key;
    public V val;
    public Entry(K key, V val);
}
*/

public class BinSearchTreePut {
    public static <K extends Comparable<K>, V> BinTree.Node<Entry<K, V>> put(
        K key, V val, BinTree.Node<Entry<K, V>> node
    ) {
        //+BEGIN_SOLUTION
        if (node == null) return new BinTree.Node<>(new Entry<>(key, val));

        int cmp = key.compareTo(node.item.key);
        if      (cmp < 0) node.left  = put(key, val, node.left);
        else if (cmp > 0) node.right = put(key, val, node.right);
        else              node.item.val = val;

        node.size = 1 + BinTree.size(node.left) + BinTree.size(node.right);
        return node;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
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
                new Entry<>(8, "eight"),
                new Entry<>(9, "nine"),
                new Entry<>(10, "ten")
        };

        BinTree.Node<Entry<Integer, String>> tree = null;
        tree = put(4, "four", tree);
        tree = put(2, "two", tree);
        tree = put(5, "five", tree);
        tree = put(1, "one", tree);
        tree = put(3, "three", tree);
        tree = put(6, "six", tree);
        tree = put(7, "seven", tree);
        tree = put(8, "eight", tree);
        tree = put(9, "nine", tree);
        tree = put(10, "ten", tree);
        Assert.assertArrayEquals(arr, BinTree.toArray(tree));
        Assert.assertEquals(10, BinTree.size(tree));
        Assert.assertEquals(7, BinTree.height(tree));

        System.out.println("OK");
    }
    //+END_FOLD }
}
