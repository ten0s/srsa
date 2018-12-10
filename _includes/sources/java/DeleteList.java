/*
public class Node<T> {
    T item;
    Node<T> next;
}
*/

public class DeleteList {
    public static <T extends Comparable<T>> Node<T> delete(T key, Node<T> node) {
        // SOLUTION_BEGIN
        if (node == null) {
            return node;
        }
        if (node.item.compareTo(key) == 0) {
            node = node.next;
            return node;
        }
        Node<T> n = node;
        while (n.next != null) {
            if (n.next.item.compareTo(key) == 0) {
                n.next = n.next.next;
                break;
            } else {
                n = n.next;
            }
        }
        return node;
        // SOLUTION_END
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
