/*
public class BSTMap<Key extends Comparable<Key>, Value> {
    protected Node root;
    protected class Node {
        Key key;
        Value val;
        Node left;
        Node right;
        int size;
    }
    protected int size(Node x);
    protected Node max(Node x);
    protected Node min(Node x);
    protected Node removeMax(Node x);
    protected Node removeMin(Node x);
}
*/

public class BSTMapRemove<Key extends Comparable<Key>, Value> extends BSTMap<Key, Value> {
    public void remove(Key key) {
        //+BEGIN_SOLUTION
        root = remove(key, root);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private Node remove(Key key, Node x) {
        if (x == null) return null;
        int cmp = key.compareTo(x.key);
        if      (cmp < 0) x.left  = remove(key, x.left);
        else if (cmp > 0) x.right = remove(key, x.right);
        else {
            if (x.left == null)  return x.right;
            if (x.right == null) return x.left;
            Node t = x;
            if (Math.random() < 0.5) {
                x = max(t.left);
                x.left = removeMax(t.left);
                x.right = t.right;
            } else {
                x = min(t.right);
                x.right = removeMin(t.right);
                x.left = t.left;
            }
        }
        x.size = 1 + size(x.left) + size(x.right);
        return x;
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        BSTMapRemove<Integer, String> map = new BSTMapRemove<>();
        map.put(4, "four");
        map.put(2, "two");
        map.put(6, "six");
        map.put(1, "one");
        map.put(3, "three");
        map.put(5, "fivex");
        map.put(7, "seven");

        map.remove(1);
        Assert.assertArrayEquals(new Integer[] {2,3,4,5,6,7},
                                 ArrayUtil.toArray(map.keys()));

        map.remove(7);
        Assert.assertArrayEquals(new Integer[] {2,3,4,5,6},
                                 ArrayUtil.toArray(map.keys()));

        map.remove(4);
        Assert.assertArrayEquals(new Integer[] {2,3,5,6},
                                 ArrayUtil.toArray(map.keys()));

        System.out.println("OK");
    }
    //+END_FOLD }
}
