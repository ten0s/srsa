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

public class BSTMapRemoveMinMax<Key extends Comparable<Key>, Value> extends BSTMap<Key, Value> {
    public void removeMin() {
        //+BEGIN_SOLUTION
        if (isEmpty()) throw new NoSuchElementException();
        root = removeMin(root);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    protected Node removeMin(Node x) {
        if (x.left == null) return x.right;
        x.left = removeMin(x.left);
        x.size = 1 + size(x.left) + size(x.right);
        return x;
    }
    //+END_SOLUTION

    public void removeMax() {
        //+BEGIN_SOLUTION
        if (isEmpty()) throw new NoSuchElementException();
        root = removeMax(root);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    protected Node removeMax(Node x) {
        if (x.right == null) return x.left;
        x.right = removeMax(x.right);
        x.size = 1 + size(x.left) + size(x.right);
        return x;
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        BSTMapRemoveMinMax<Integer, String> map = new BSTMapRemoveMinMax<>();

        try { map.removeMin(); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        try { map.removeMax(); Assert.assertTrue(false); } catch (NoSuchElementException e) {}

        map.put(4, "four");
        map.put(2, "two");
        map.put(6, "six");
        map.put(1, "one");
        map.put(3, "three");
        map.put(5, "fivex");
        map.put(7, "seven");

        map.removeMin();
        Assert.assertArrayEquals(new Integer[] {2,3,4,5,6,7},
                                 ArrayUtil.toArray(map.keys()));
        map.removeMin();
        Assert.assertArrayEquals(new Integer[] {3,4,5,6,7},
                                 ArrayUtil.toArray(map.keys()));

        map.removeMax();
        Assert.assertArrayEquals(new Integer[] {3,4,5,6},
                                 ArrayUtil.toArray(map.keys()));

        map.removeMax();
        Assert.assertArrayEquals(new Integer[] {3,4,5},
                                 ArrayUtil.toArray(map.keys()));

        System.out.println("OK");
    }
    //+END_FOLD }
}
