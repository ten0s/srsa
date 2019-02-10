/*
public class Node<T> {
    T item;
    Node<T> next;
}
*/

public class ListDelete {
    public static <T extends Comparable<T>> Node<T> delete(T key, Node<T> node) {
        // BEGIN_SOLUTION
        if (node == null) {
            return node;
        }
        if (key.compareTo(node.item) == 0) {
            return node.next;
        }
        for (Node<T> n = node; n.next != null; n = n.next) {
            if (key.compareTo(n.next.item) == 0) {
                n.next = n.next.next;
                break;
            }
        }
        return node;
        // END_SOLUTION
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertArrayEquals(new int[] {}, Node.toIntArray(delete(1, null)));

        Node<Integer> n = Node.fromIntArray(new int[] {1,2,3,3,4,5});
        Assert.assertArrayEquals(new int[] {1,2,3,3,4,5}, Node.toIntArray(n));

        n = delete(1, n);
        Assert.assertArrayEquals(new int[] {2,3,3,4,5}, Node.toIntArray(n));
        n = delete(3, n);
        Assert.assertArrayEquals(new int[] {2,3,4,5}, Node.toIntArray(n));
        n = delete(5, n);
        Assert.assertArrayEquals(new int[] {2,3,4}, Node.toIntArray(n));
        n = delete(6, n);
        Assert.assertArrayEquals(new int[] {2,3,4}, Node.toIntArray(n));

        System.out.println("OK");
    }
}
