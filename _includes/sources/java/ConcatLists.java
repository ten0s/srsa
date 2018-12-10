/*
public class Node<T> {
    T item;
    Node<T> next;
}
*/

public class ConcatLists {
    public static <T> Node<T> concat(Node<T> n1, Node<T> n2) {
        // SOLUTION_BEGIN
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
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        // [1,2,3]
        Node<Integer> n1 = new Node<>();
        n1.item = 1;
        Node<Integer> n2 = new Node<>();
        n2.item = 2;
        n1.next = n2;
        Node<Integer> n3 = new Node<>();
        n3.item = 3;
        n2.next = n3;

        // [4,5,6]
        Node<Integer> n4 = new Node<>();
        n4.item = 4;
        Node<Integer> n5 = new Node<>();
        n5.item = 5;
        n4.next = n5;
        Node<Integer> n6 = new Node<>();
        n6.item = 6;
        n5.next = n6;

        Assert.assertArrayEquals(new int[] {}, Node.toIntArray(concat(null, null)));
        Assert.assertArrayEquals(new int[] {1,2,3}, Node.toIntArray(concat(n1, null)));
        Assert.assertArrayEquals(new int[] {4,5,6}, Node.toIntArray(concat(null, n4)));
        Assert.assertArrayEquals(new int[] {1,2,3,4,5,6}, Node.toIntArray(concat(n1, n4)));

        System.out.println("OK");
    }
}
