/*
public class Node<T> {
    T item;
    Node<T> next;
*/

public class Main {
    public static Node reverse(Node node) {
        // SOLUTION_BEGIN
        Node prev = null;
        Node curr = node;
        Node next = null;
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

        Node<Integer> n = new Node<>();
        n.item = 1;

        Assert.assertArrayEquals(new int[] {1}, Node.toIntArray(reverse(n)));

        Node<Integer> n2 = new Node<>();
        n2.item = 2;
        n.next = n2;

        Node<Integer> n3 = new Node<>();
        n3.item = 3;
        n2.next = n3;

        Node<Integer> n4 = new Node<>();
        n4.item = 4;
        n3.next = n4;

        Node<Integer> n5 = new Node<>();
        n5.item = 5;
        n4.next = n5;

        Assert.assertArrayEquals(new int[] {1,2,3,4,5}, Node.toIntArray(n));
        Assert.assertArrayEquals(new int[] {5,4,3,2,1}, Node.toIntArray(reverse(n)));

        System.out.println("OK");
    }
}
