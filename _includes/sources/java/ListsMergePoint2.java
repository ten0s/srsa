import java.util.*;

/*
public class Node<T> {
    T item;
    Node<T> next;
}
*/

public class ListsMergePoint2 {
    public static <T> Node<T> mergePoint(Node<T> n1, Node<T> n2) {
        //+BEGIN_SOLUTION
        Deque<Node<T>> s1 = new ArrayDeque<>();
        Deque<Node<T>> s2 = new ArrayDeque<>();
        while (n1 != null) {
            s1.push(n1);
            n1 = n1.next;
        }
        while (n2 != null) {
            s2.push(n2);
            n2 = n2.next;
        }
        while (!s1.isEmpty() && !s2.isEmpty()) {
            n1 = s1.pop();
            n2 = s2.pop();
            if (n1 != n2) return n1.next;
        }
        return null;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
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

        Node<Integer> n9 = new Node<>(9);
        Node<Integer> n8 = new Node<>(8);
        n9.next = n8;
        Node<Integer> n7 = new Node<>(7);
        n8.next = n7;
        n7.next = n3;

        Assert.assertEquals(n3, mergePoint(n1, n9));

        System.out.println("OK");
    }
    //+END_FOLD }
}
