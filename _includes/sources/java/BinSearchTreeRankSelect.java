/*
public class BinTree {
    public static class Node<T> {
        public T item;
        public Node<T> left;
        public Node<T> right;
    }
    public static int size(Node<T> node);
}
*/

public class BinSearchTreeRankSelect {
    public static <K extends Comparable<K>> int rank(K key, BinTree.Node<K> node) {
        //+BEGIN_SOLUTION
        if (node == null) return 0;
        int cmp = key.compareTo(node.item);
        if      (cmp < 0) return rank(key, node.left);
        else if (cmp > 0) return 1 + BinTree.size(node.left) + rank(key, node.right);
        else              return BinTree.size(node.left);
        //+END_SOLUTION
    }

    public static <K extends Comparable<K>> BinTree.Node<K> select(int k, BinTree.Node<K> node) {
        //+BEGIN_SOLUTION
        if (node == null) return null;
        int r = BinTree.size(node.left);
        if      (r > k) return select(k, node.left);
        else if (r < k) return select(k-r-1, node.right);
        else            return node;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        int[] keys = new int[] {1,3,5,6,7,9};
        BinTree.Node<Integer> tree = BinTree.fromIntArray(keys);
        //BinTree.println(tree);

        // Property #1
        // for i in [0..size(keys)-1] { i == rank(select(i)) }
        for (int i = 0; i < keys.length; i++) {
            Assert.assertEquals(i, (int)rank(select(i, tree).item, tree));
        }

        // Property #2
        // for all key in keys { key == select(rank(key)) }
        for (int key : keys) {
            Assert.assertEquals(key, (int)select(rank(key, tree), tree).item);
        }

        System.out.println("OK");
    }
    //+END_FOLD }
}
