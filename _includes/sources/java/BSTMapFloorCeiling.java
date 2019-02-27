import java.util.NoSuchElementException;

/*
public class BSTMap<Key extends Comparable<Key>, Value> {
    protected Node root;
    protected class Node {
        Key key;
        Node left;
        Node right;
    }
}
*/

public class BSTMapFloorCeiling<Key extends Comparable<Key>, Value> extends BSTMap<Key, Value> {
    public Key floor(Key key) {
        //+BEGIN_SOLUTION
        Node x = floor(key, root);
        if (x == null) throw new NoSuchElementException();
        return x.key;
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private Node floor(Key key, Node x) {
        if (x == null) return null;
        int cmp = key.compareTo(x.key);
        if (cmp == 0) return x;
        if (cmp < 0) return floor(key, x.left);
        Node t = floor(key, x.right);
        if (t != null) return t;
        else           return x;
    }
    //+END_SOLUTION

    public Key ceiling(Key key) {
        //+BEGIN_SOLUTION
        Node x = ceiling(key, root);
        if (x == null) throw new NoSuchElementException();
        return x.key;
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private Node ceiling(Key key, Node x) {
        if (x == null) return null;
        int cmp = key.compareTo(x.key);
        if (cmp == 0) return x;
        if (cmp > 0) return ceiling(key, x.right);
        Node t = ceiling(key, x.left);
        if (t != null) return t;
        else           return x;
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        BSTMapFloorCeiling<Integer, String> map = new BSTMapFloorCeiling<>();
        // test empty map
        try { map.floor(1); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        try { map.ceiling(1); Assert.assertTrue(false); } catch (NoSuchElementException e) {}

        map.put(5, "five");
        map.put(3, "three");
        map.put(7, "seven");
        map.put(1, "one");
        map.put(6, "six");
        map.put(9, "nine");

        // test floor
        try { map.floor(0); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        Assert.assertEquals(1, (int)map.floor(1));
        Assert.assertEquals(1, (int)map.floor(2));
        Assert.assertEquals(3, (int)map.floor(3));
        Assert.assertEquals(6, (int)map.floor(6));
        Assert.assertEquals(7, (int)map.floor(8));
        Assert.assertEquals(9, (int)map.floor(9));
        Assert.assertEquals(9, (int)map.floor(10));

        // test ceiling
        try { map.ceiling(10); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        Assert.assertEquals(1, (int)map.ceiling(0));
        Assert.assertEquals(1, (int)map.ceiling(1));
        Assert.assertEquals(3, (int)map.ceiling(2));
        Assert.assertEquals(5, (int)map.ceiling(4));
        Assert.assertEquals(9, (int)map.ceiling(8));
        Assert.assertEquals(9, (int)map.ceiling(9));

        System.out.println("OK");
    }
    //+END_FOLD }
}
