/*
public class Node<T> {
    T item;
    Node<T> next;
}
*/

public class ListsMergePoint {
    public static <T> Node<T> mergePoint(Node<T> l1, Node<T> l2) {
        // BEGIN_SOLUTION
        if (l1 == null || l2 == null) return null;
        if (l1 == l2) return l1;
        Node<T> n1 = l1,    n2 = l2;
        boolean f1 = false, f2 = false;
        while (true) {
            n1 = n1.next;
            n2 = n2.next;
            if (n1 == null) {
                if (f1) break;
                n1 = l2;
                f1 = !f1;
            }
            if (n2 == null) {
                if (f2) break;
                n2 = l1;
                f2 = !f2;
            }
            if (n1 == n2) return n1;
        }
        return null;
        // END_SOLUTION
    }

    // BEGIN_SOLUTION
    public static <T> Node<T> mergePoint2(Node<T> l1, Node<T> l2) {
        Stack<Node<T>> s1 = new Stack<>();
        Stack<Node<T>> s2 = new Stack<>();
        while (l1 != null || l2 != null) {
            if (l1 != null) {
                s1.push(l1);
                l1 = l1.next;
            }
            if (l2 != null) {
                s2.push(l2);
                l2 = l2.next;
            }
        }
        while (!s1.isEmpty() && !s2.isEmpty()) {
            Node<T> n1 = s1.pop();
            Node<T> n2 = s2.pop();
            if (n1 != n2) return n1.next;
        }
        return null;
    }
    // END_SOLUTION

    public static void main(String[] args) throws Throwable {
        Assert.assertNull(mergePoint(null, Node.fromIntArray(new int[] {1,2,3})));
        Assert.assertNull(mergePoint(Node.fromIntArray(new int[] {1,2,3}), null));
        Assert.assertNull(mergePoint(Node.fromIntArray(new int[] {1,2,3}),
                                     Node.fromIntArray(new int[] {4,5,6,7})));

        Node<Integer> n1 = new Node<>(1);
        Node<Integer> n2 = new Node<>(2);
        n1.next = n2;
        Node<Integer> n3 = new Node<>(3);
        n2.next = n3;
        Node<Integer> n4 = new Node<>(4);
        n3.next = n4;
        Node<Integer> n5 = new Node<>(5);
        n4.next = n5;

        Node<Integer> nA = new Node<>(Character.getNumericValue('A'));
        Node<Integer> nB = new Node<>(Character.getNumericValue('B'));
        nA.next = nB;
        Node<Integer> nC = new Node<>(Character.getNumericValue('C'));
        nB.next = nC;
        nC.next = n3;

        Assert.assertEquals(n3, mergePoint(n1, nA));

        System.out.println("OK");
    }
}

/*
public class public class Stack<Item> implements Iterable<Item> {
    public Stack();
    public void push(Item item);
    public Item pop();
}
*/
