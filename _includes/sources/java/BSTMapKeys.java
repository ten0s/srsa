import java.util.*;

/*
public class BSTMap<Key extends Comparable<Key>, Value> {
    protected Node root;
    protected class Node {
        Key key;
        Value val;
        Node left;
        Node right;
    }
    public boolean isEmpty();
    public Key min();
    public key max();
}
*/

public class BSTMapKeys<Key extends Comparable<Key>, Value> extends BSTMap<Key, Value> {
    public Iterable<Key> keys() {
        //+BEGIN_SOLUTION
        if (isEmpty()) {
            return new Iterable<Key>() {
                public Iterator<Key> iterator() {
                    return Collections.emptyIterator();
                }
            };
        }
        return keys(min(), max());
        //+END_SOLUTION
    }

    public Iterable<Key> keys(Key lo, Key hi) {
        //+BEGIN_SOLUTION
        Deque<Key> queue = new ArrayDeque<>();
        keys(root, lo, hi, queue);
        return queue;
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private void keys(Node x, Key lo, Key hi, Deque<Key> queue) {
        if (x == null) return;
        int cmpLo = lo.compareTo(x.key);
        int cmpHi = hi.compareTo(x.key);
        // in-order traversal
        if (cmpLo < 0) keys(x.left, lo, hi, queue);
        if (cmpLo <= 0 && cmpHi >= 0) queue.add(x.key);
        if (cmpHi > 0) keys(x.right, lo, hi, queue);
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        BSTMapKeys<Integer, String> map = new BSTMapKeys<>();
        Assert.assertArrayEquals(new Integer[] {},
                                 ArrayUtil.toArray(map.keys()));

        map.put(4, "four");
        map.put(2, "two");
        map.put(6, "six");
        map.put(1, "one");
        map.put(3, "three");
        map.put(5, "five");
        map.put(7, "seven");

        Assert.assertArrayEquals(new Integer[] {1,2,3,4,5,6,7},
                                 ArrayUtil.toArray(map.keys()));

        Assert.assertArrayEquals(new Integer[] {2,3,4,5,6},
                                 ArrayUtil.toArray(map.keys(2, 6)));

        System.out.println("OK");
    }
    //+END_FOLD }
}
