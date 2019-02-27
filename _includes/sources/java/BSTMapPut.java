/*
public class BSTMap<Key extends Comparable<Key>, Value> {
    protected Node root;
    protected class Node {
        Key key;
        Value val;
        Node left;
        Node right;
        int size;
        public Node(Key key, Value val) {
            this.key = key;
            this.val = val;
            this.size = 1;
        }
    }
    protected int size(Node x);
}
*/

public class BSTMapPut<Key extends Comparable<Key>, Value> extends BSTMap<Key, Value> {
    public void put(Key key, Value val) {
        //+BEGIN_SOLUTION
        root = put(key, val, root);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private Node put(Key key, Value val, Node x) {
        if (x == null) return new Node(key, val);
        int cmp = key.compareTo(x.key);
        if      (cmp < 0) x.left  = put(key, val, x.left);
        else if (cmp > 0) x.right = put(key, val, x.right);
        else              x.val = val;
        x.size = 1 + size(x.left) + size(x.right);
        return x;
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        BSTMapPut<Integer, String> map = new BSTMapPut<>();
        map.put(4, "four");
        map.put(2, "two");
        map.put(5, "five");
        map.put(1, "one");
        map.put(3, "three");
        map.put(6, "six");
        map.put(7, "seven");
        map.put(8, "eight");
        map.put(9, "nine");
        map.put(10, "ten");
        Assert.assertArrayEquals(new Integer[] {1,2,3,4,5,6,7,8,9,10},
                                 ArrayUtil.toArray(map.keys()));
        Assert.assertEquals(10, map.size());
        Assert.assertEquals(7, map.height());

        Assert.assertNull(map.get(0));
        Assert.assertEquals("one", map.get(1));
        Assert.assertEquals("two", map.get(2));
        Assert.assertEquals("four", map.get(4));
        Assert.assertEquals("six", map.get(6));
        Assert.assertEquals("seven", map.get(7));
        Assert.assertEquals("ten", map.get(10));
        Assert.assertNull(map.get(11));

        System.out.println("OK");
    }
    //+END_FOLD }
}
