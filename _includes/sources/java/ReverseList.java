/*
public class Node<T> {
    T item;
    Node<T> next;
*/

public class ReverseList {
    public static <T> Node<T> reverse(Node<T> node) {
        // SOLUTION_BEGIN
        Node<T> prev = null;
        Node<T> curr = node;
        Node<T> next = null;
        while (curr != null) {
            next = curr.next;
            curr.next = prev;
            prev = curr;
            curr = next;
        }
        return prev;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertArrayEquals(new int[] {}, Node.toIntArray(reverse(null)));

        Node<Integer> n = new Node<>(1);

        Assert.assertArrayEquals(new int[] {1}, Node.toIntArray(reverse(n)));

        Node<Integer> n2 = new Node<>(2);
        n.next = n2;
        Node<Integer> n3 = new Node<>(3);
        n2.next = n3;
        Node<Integer> n4 = new Node<>(4);
        n3.next = n4;
        Node<Integer> n5 = new Node<>(5);
        n4.next = n5;

        Assert.assertArrayEquals(new int[] {1,2,3,4,5}, Node.toIntArray(n));
        Assert.assertArrayEquals(new int[] {5,4,3,2,1}, Node.toIntArray(reverse(n)));

        System.out.println("OK");
    }
}
