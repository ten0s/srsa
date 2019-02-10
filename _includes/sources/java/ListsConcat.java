/*
public class Node<T> {
    T item;
    Node<T> next;
}
*/

public class ListsConcat {
    public static <T> Node<T> concat(Node<T> n1, Node<T> n2) {
        // BEGIN_SOLUTION
        if (n1 == null) {
            return n2;
        }
        if (n2 == null) {
            return n1;
        }
        Node<T> n = n1;
        while (n.next != null) {
            n = n.next;
        }
        n.next = n2;
        return n1;
        // END_SOLUTION
    }

    public static void main(String[] args) throws Throwable {
        Node<Integer> n1 = Node.fromIntArray(new int[] {1,2,3});
        Node<Integer> n4 = Node.fromIntArray(new int[] {4,5,6});
        Assert.assertArrayEquals(new int[] {}, Node.toIntArray(concat(null, null)));
        Assert.assertArrayEquals(new int[] {1,2,3}, Node.toIntArray(concat(n1, null)));
        Assert.assertArrayEquals(new int[] {4,5,6}, Node.toIntArray(concat(null, n4)));
        Assert.assertArrayEquals(new int[] {1,2,3,4,5,6}, Node.toIntArray(concat(n1, n4)));
        System.out.println("OK");
    }
}
