import java.util.NoSuchElementException;

/*
public class BSTMap<Key extends Comparable<Key>, Value> {
    protected Node root;
    protected class Node {
        Key key;
        Node left;
        Node right;
    }
    protected int size(Node x);
}
*/

public class BSTMapRankSelect<Key extends Comparable<Key>, Value> extends BSTMap<Key, Value> {
    public int rank(Key key) {
        //+BEGIN_SOLUTION
        return rank(root, key);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private int rank(Node x, Key key) {
        if (x == null) return 0;
        int cmp = key.compareTo(x.key);
        if      (cmp < 0) return rank(x.left, key);
        else if (cmp > 0) return 1 + size(x.left) + rank(x.right, key);
        else              return size(x.left);
    }
    //+END_SOLUTION

    public Key select(int k) {
        //+BEGIN_SOLUTION
        if (k < 0 || k >= size())
            throw new IllegalArgumentException();
        return select(root, k).key;
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private Node select(Node x, int k) {
        if (x == null) return null;
        int t = size(x.left);
        if      (t > k) return select(x.left, k);
        else if (t < k) return select(x.right, k-t-1);
        else            return x;
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        BSTMapRankSelect<Integer, String> map = new BSTMapRankSelect<>();
        map.put(5, "five");
        map.put(3, "three");
        map.put(7, "seven");
        map.put(1, "one");
        map.put(6, "six");
        map.put(9, "nine");

        // Property #1
        // for i in [0..size(keys)-1] { i == rank(select(i)) }
        for (int i = 0; i < map.size(); i++) {
            Assert.assertEquals(i, (int)map.rank(map.select(i)));
        }

        // Property #2
        // for all key in keys { key == select(rank(key)) }
        for (int key : map.keys()) {
            Assert.assertEquals(key, (int)map.select(map.rank(key)));
        }

        System.out.println("OK");
    }
    //+END_FOLD }
}
