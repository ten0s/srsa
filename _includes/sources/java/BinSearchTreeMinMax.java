import java.util.NoSuchElementException;

/*
public class BinTree.Node<T> {
    public T item;
    public Node<T> left;
    public Node<T> right;
}
*/

public class BinSearchTreeMinMax {
    public static <K extends Comparable<K>> K min(BinTree.Node<K> n) {
        //+BEGIN_SOLUTION
        if (n == null)
            throw new NoSuchElementException();
        if (n.left == null)
            return n.item;
        return min(n.left);
        //+END_SOLUTION
    }

    public static <K extends Comparable<K>> K max(BinTree.Node<K> n) {
        //+BEGIN_SOLUTION
        if (n == null)
            throw new NoSuchElementException();
        if (n.right == null)
            return n.item;
        return max(n.right);
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        BinTree.Node<Integer> tree = BinTree.fromIntArray(new int[] {1,2,3,4,5,6,7});

        try { min(null); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        Assert.assertEquals(1, min(tree));

        try { max(null); Assert.assertTrue(false); } catch (NoSuchElementException e) {}
        Assert.assertEquals(7, max(tree));

        System.out.println("OK");
    }
    //+END_FOLD }
}
