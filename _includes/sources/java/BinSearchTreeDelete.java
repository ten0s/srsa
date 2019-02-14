import java.util.NoSuchElementException;

/*
public class BinTree.Node<T> {
    public T item;
    public Node<T> left;
    public Node<T> right;
}
*/

public class BinSearchTreeDelete {
    public static <K extends Comparable<K>> BinTree.Node<K> delete(K key, BinTree.Node<K> node) {
        //+BEGIN_SOLUTION
        if (node == null) return node;
        int cmp = key.compareTo(node.item);
        if      (cmp < 0) node.left  = delete(key, node.left);
        else if (cmp > 0) node.right = delete(key, node.right);
        else {
            if (node.left == null)  return node.right;
            if (node.right == null) return node.left;
            BinTree.Node<K> t = node;
            if (Math.random() < 0.5) {
                node = min(t.right);
                node.right = deleteMin(t.right);
                node.left = t.left;
            } else {
                node = max(t.left);
                node.left = deleteMax(t.left);
                node.right = t.right;
            }
        }
        return node;
        //+END_SOLUTION
    }

    public static <K extends Comparable<K>> BinTree.Node<K> min(BinTree.Node<K> node) { //+BEGIN_FOLD
        if (node == null) throw new NoSuchElementException();
        if (node.left == null)
            return node;
        return min(node.left);
    } //+END_FOLD To Use

    public static <K extends Comparable<K>> BinTree.Node<K> max(BinTree.Node<K> node) { //+BEGIN_FOLD
        if (node == null) throw new NoSuchElementException();
        if (node.right == null)
            return node;
        return max(node.right);
    } //+END_FOLD To Use

    public static <K extends Comparable<K>> BinTree.Node<K> deleteMin(BinTree.Node<K> node) { //+BEGIN_FOLD
        if (node == null) throw new NoSuchElementException();
        if (node.left == null)
            return node.right;
        node.left = deleteMin(node.left);
        return node;
    } //+END_FOLD To Use

    public static <K extends Comparable<K>> BinTree.Node<K> deleteMax(BinTree.Node<K> node) { //+BEGIN_FOLD
        if (node == null) throw new NoSuchElementException();
        if (node.right == null)
            return node.left;
        node.right = deleteMax(node.right);
        return node;
    } //+END_FOLD To Use

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Assert.assertNull(delete(1, null));

        BinTree.Node<Integer> tree;
        tree = BinTree.fromIntArray(new int[] {1,2,3,4,5,6,7});
        tree = delete(1, tree);
        Assert.assertArrayEquals(new int[] {2,3,4,5,6,7}, BinTree.toIntArray(tree));
        tree = delete(7, tree);
        Assert.assertArrayEquals(new int[] {2,3,4,5,6}, BinTree.toIntArray(tree));
        tree = delete(4, tree);
        Assert.assertArrayEquals(new int[] {2,3,5,6}, BinTree.toIntArray(tree));

        System.out.println("OK");
    }
    //+END_FOLD }
}
