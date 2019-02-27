/*
public class BSTMap<Key extends Comparable<Key>, Value> {
    protected Node root;
    protected class Node {
        Key key;
        Value val;
        Node left;
        Node right;
    }
}
*/

public class BSTMapGet<Key extends Comparable<Key>, Value> extends BSTMap<Key, Value> {
    public Value get(Key key) {
        //+BEGIN_SOLUTION
        return get(root, key);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private Value get(Node x, Key key) {
        if (x == null) return null;
        int cmp = key.compareTo(x.key);
        if      (cmp < 0) return get(x.left, key);
        else if (cmp > 0) return get(x.right, key);
        else              return x.val;
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        BSTMapGet<Integer, String> map = new BSTMapGet<>();
        map.put(4, "four");
        map.put(2, "two");
        map.put(6, "six");
        map.put(1, "one");
        map.put(3, "three");
        map.put(5, "five");
        map.put(7, "seven");

        Assert.assertNull(map.get(0));
        Assert.assertEquals("one", map.get(1));
        Assert.assertEquals("two", map.get(2));
        Assert.assertEquals("four", map.get(4));
        Assert.assertEquals("six", map.get(6));
        Assert.assertEquals("seven", map.get(7));
        Assert.assertNull(map.get(8));

        System.out.println("OK");
    }
    //+END_FOLD }
}
