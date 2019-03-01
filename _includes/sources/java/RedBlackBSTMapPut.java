/*
public class RedBlackBSTMap<Key extends Comparable<Key>, Value> {
    protected static final boolean RED   = true;
    protected static final boolean BLACK = false;

    protected Node root;
    protected class Node {
        Key key;
        Value val;
        Node left;
        Node right;
        int size;
        boolean color;
        public Node(Key key, Value val, boolean color) {
            this.key = key;
            this.val = val;
            this.size = 1;
            this.color = color;
        }
    }
    protected int size(Node x);
}
*/

public class RedBlackBSTMapPut<Key extends Comparable<Key>, Value> extends RedBlackBSTMap<Key, Value> {
    public void put(Key key, Value val) {
        //+BEGIN_SOLUTION
        root = put(root, key, val);
        root.color = BLACK;
        //+END_SOLUTION
    }

    private Node put(Node h, Key key, Value val) {
        //+BEGIN_SOLUTION
        if (h == null) return new Node(key, val, RED);

        int cmp = key.compareTo(h.key);
        if      (cmp < 0) h.left  = put(h.left, key, val);
        else if (cmp > 0) h.right = put(h.right, key, val);
        else              h.val = val;

        if (!isRed(h.left) && isRed(h.right))     h = rotateLeft(h);
        if ( isRed(h.left) && isRed(h.left.left)) h = rotateRight(h);
        if ( isRed(h.left) && isRed(h.right))     flipColors(h);

        h.size = 1 + size(h.left) + size(h.right);
        return h;
        //+END_SOLUTION
    }

    private boolean isRed(Node x) {
        //+BEGIN_SOLUTION
        // null links are black
        if (x == null) return false;
        return x.color == RED;
        //+END_SOLUTION
    }

    private Node rotateLeft(Node h) {
        //+BEGIN_SOLUTION
        assert isRed(h.right);
        Node x = h.right;
        h.right = x.left;
        x.left = h;
        x.color = h.color;
        h.color = RED;
        x.size = h.size;
        h.size = 1 + size(h.left) + size(h.right);
        return x;
        //+END_SOLUTION
    }

    private Node rotateRight(Node h) {
        //+BEGIN_SOLUTION
        assert isRed(h.left);
        Node x = h.left;
        h.left = x.right;
        x.right = h;
        x.color = h.color;
        h.color = RED;
        x.size = h.size;
        h.size = 1 + size(h.left) + size(h.right);
        return x;
        //+END_SOLUTION
    }

    private void flipColors(Node h) {
        //+BEGIN_SOLUTION
        assert !isRed(h);
        assert isRed(h.left);
        assert isRed(h.right);
        h.color = RED;
        h.left.color = BLACK;
        h.right.color = BLACK;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        RedBlackBSTMapPut<Integer, String> map = new RedBlackBSTMapPut<>();
        map.put(1, "one");
        map.put(2, "two");
        map.put(3, "three");
        map.put(4, "four");
        map.put(5, "five");
        map.put(6, "six");
        map.put(7, "seven");
        map.put(8, "eight");
        map.put(9, "nine");
        map.put(10, "ten");

        Assert.assertEquals(BLACK, map.root.color);
        Assert.assertArrayEquals(new Integer[] {1,2,3,4,5,6,7,8,9,10},
                                 ArrayUtil.toArray(map.keys()));
        Assert.assertEquals(10, map.size());
        Assert.assertEquals(4, map.height());

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
