import java.util.NoSuchElementException;

/*
public class BSTMap<Key extends Comparable<Key>, Value> {
    protected Node root;
    protected class Node {
        Key key;
        Node left;
        Node right;
    }
    public boolean isEmpty();
}
*/

public class BSTMapMinMax<Key extends Comparable<Key>, Value> extends BSTMap<Key, Value> {
    public Key min() {
        //+BEGIN_SOLUTION
        if (isEmpty()) throw new NoSuchElementException();
        return min(root).key;
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    protected Node min(Node x) {
        if (x.left == null) return x;
        return min(x.left);
    }
    //+END_SOLUTION

    public Key max() {
        //+BEGIN_SOLUTION
        if (isEmpty()) throw new NoSuchElementException();
        return max(root).key;
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    protected Node max(Node x) {
        if (x.right == null) return x;
        return max(x.right);
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        BSTMapMinMax<Integer, String> map = new BSTMapMinMax<>();

        try { map.min(); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        try { map.max(); Assert.assertTrue(false); } catch (NoSuchElementException e) {}

        map.put(4, "four");
        map.put(2, "two");
        map.put(6, "six");
        map.put(1, "one");
        map.put(3, "three");
        map.put(5, "fivex");
        map.put(7, "seven");

        Assert.assertEquals(1, (int)map.min());
        Assert.assertEquals(7, (int)map.max());

        System.out.println("OK");
    }
    //+END_FOLD }
}
