/*
public class Node<T> {
    T item;
    Node<T> next;
}

public class Pair<A, B> {
    public A first;
    public B second;
}
*/

public class SplitListInTwo {
    public static <T> Pair<Node<T>, Node<T>> split(Node<T> node) {
        // SOLUTION_BEGIN
        if (node == null) {
            throw new IllegalArgumentException();
        }
        Node<T> slow = node;
        Node<T> fast = node;
        while (fast.next != null && fast.next.next != null) {
            slow = slow.next;
            fast = fast.next.next;
        }
        Pair<Node<T>, Node<T>> subs = new Pair<>();
        subs.first = node;
        subs.second = slow.next;
        slow.next = null;
        return subs;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        try {
            split(null);
            Assert.assertTrue(false);
        } catch (IllegalArgumentException e) {}

        Pair<Node<Integer>, Node<Integer>> subs;
        subs = split(Node.fromIntArray(new int[] {1}));
        Assert.assertArrayEquals(new int[] {1}, Node.toIntArray(subs.first));
        Assert.assertNull(subs.second);

        subs = split(Node.fromIntArray(new int[] {1,2}));
        Assert.assertArrayEquals(new int[] {1}, Node.toIntArray(subs.first));
        Assert.assertArrayEquals(new int[] {2}, Node.toIntArray(subs.second));

        subs = split(Node.fromIntArray(new int[] {1,2,3}));
        Assert.assertArrayEquals(new int[] {1,2}, Node.toIntArray(subs.first));
        Assert.assertArrayEquals(new int[] {3}, Node.toIntArray(subs.second));

        System.out.println("OK");
    }
}
