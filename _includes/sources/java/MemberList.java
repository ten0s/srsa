/*
public class Node<T> {
    T item;
    Node<T> next;
}
*/

public class MemberList {
    public static <T extends Comparable<T>>boolean isMember(T key, Node<T> node) {
        // SOLUTION_BEGIN
        for (Node<T> n = node; n != null; n = n.next) {
            if (n.item.compareTo(key) == 0)
                return true;
        }
        return false;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        Node<Integer> n = null;
        Assert.assertFalse(isMember(1, n));

        n = new Node<>();
        n.item = 1;

        Node<Integer> n2 = new Node<>();
        n2.item = 2;
        n.next = n2;

        Node<Integer> n3 = new Node<>();
        n3.item = 3;
        n2.next = n3;

        Assert.assertTrue(isMember(1, n));
        Assert.assertTrue(isMember(2, n));
        Assert.assertTrue(isMember(3, n));
        Assert.assertFalse(isMember(4, n));

        System.out.println("OK");
    }
}
