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
        Node<Integer> n = null;
        n = delete(1, n);
        Assert.assertArrayEquals(new int[] {}, Node.toIntArray(n));

        n = new Node<>();
        n.item = 1;

        Node<Integer> n2 = new Node<>();
        n2.item = 2;
        n.next = n2;

        Node<Integer> n3 = new Node<>();
        n3.item = 3;
        n2.next = n3;

        Node<Integer> n4 = new Node<>();
        n4.item = 3;
        n3.next = n4;

        Node<Integer> n5 = new Node<>();
        n5.item = 4;
        n4.next = n5;

        Node<Integer> n6 = new Node<>();
        n6.item = 5;
        n5.next = n6;

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
