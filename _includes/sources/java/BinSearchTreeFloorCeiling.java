import java.util.NoSuchElementException;

/*
public class BinTree.Node<T> {
    public T item;
    public Node<T> left;
    public Node<T> right;
}
*/

public class BinSearchTreeFloorCeiling {
    public static <K extends Comparable<K>> BinTree.Node<K> floor(K key, BinTree.Node<K> node) {
        //+BEGIN_SOLUTION
        if (node == null) throw new NoSuchElementException();
        int cmp = key.compareTo(node.item);
        if (cmp == 0) return node;
        if (cmp < 0) return floor(key, node.left);
        try {
            return floor(key, node.right);
        } catch (NoSuchElementException e) {
            return node;
        }
        //+END_SOLUTION
    }

    public static <K extends Comparable<K>> BinTree.Node<K> ceiling(K key, BinTree.Node<K> node) {
        //+BEGIN_SOLUTION
        if (node == null) throw new NoSuchElementException();
        int cmp = key.compareTo(node.item);
        if (cmp == 0) return node;
        if (cmp > 0) return ceiling(key, node.right);
        try {
            return ceiling(key, node.left);
        } catch (NoSuchElementException e) {
            return node;
        }
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        BinTree.Node<Integer> tree = BinTree.fromIntArray(new int[] {1,3,5,6,7,9});

        // test floor
        try { floor(1, null); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        try { floor(0, null); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        Assert.assertEquals(1, floor(1, tree).item);
        Assert.assertEquals(1, floor(2, tree).item);
        Assert.assertEquals(3, floor(3, tree).item);
        Assert.assertEquals(6, floor(6, tree).item);
        Assert.assertEquals(7, floor(8, tree).item);
        Assert.assertEquals(9, floor(9, tree).item);
        Assert.assertEquals(9, floor(10, tree).item);

        // test ceiling
        try { ceiling(1, null); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        try { ceiling(10, tree); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        Assert.assertEquals(1, ceiling(0, tree).item);
        Assert.assertEquals(1, ceiling(1, tree).item);
        Assert.assertEquals(3, ceiling(2, tree).item);
        Assert.assertEquals(5, ceiling(4, tree).item);
        Assert.assertEquals(9, ceiling(8, tree).item);
        Assert.assertEquals(9, ceiling(9, tree).item);

        System.out.println("OK");
    }
    //+END_FOLD }
}
