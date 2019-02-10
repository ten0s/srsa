/*
public class Node<T> {
    T item;
    Node<T> next;

    public static <T> Pair<Node<T>, Node<T>> split(Node<T> node);
}

public class Pair<A, B> {
    public A first;
    public B second;
}
*/

class ListMergeSort {
    public static <T extends Comparable<T>> Node<T> sort(Node<T> node) {
        // BEGIN_SOLUTION
        if (node == null || node.next == null)
            return node;
        Pair<Node<T>, Node<T>> sub = Node.split(node);
        Node<T> left = sort(sub.first);
        Node<T> right = sort(sub.second);
        return merge(left, right);
        // END_SOLUTION
    }

    // BEGIN_SOLUTION
    private static <T extends Comparable<T>> Node<T> merge(Node<T> left, Node<T> right) {
        if (left == null)
            return right;
        if (right == null)
            return left;
        if (less(right.item, left.item)) {
            right.next = merge(left, right.next);
            return right;
        } else {
            left.next = merge(left.next, right);
            return left;
        }
    }
    // END_SOLUTION

    private static <T extends Comparable<T>> boolean less(T v, T w) {
        return v.compareTo(w) < 0;
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertArrayEquals(new int[] {},
                                 Node.toIntArray(
                                     sort(
                                         Node.fromIntArray(
                                             new int[] {}))));
        Assert.assertArrayEquals(new int[] {1},
                                 Node.toIntArray(
                                     sort(
                                         Node.fromIntArray(
                                             new int[] {1}))));
        Assert.assertArrayEquals(new int[] {1,2,3,4,5,6},
                                 Node.toIntArray(
                                     sort(
                                         Node.fromIntArray(
                                             new int[] {6,5,4,3,2,1}))));
        Assert.assertArrayEquals(new int[] {1,2,3,4,5,6},
                                 Node.toIntArray(
                                     sort(
                                         Node.fromIntArray(
                                             new int[] {1,2,3,4,5,6}))));
        Assert.assertArrayEquals(new int[] {1,2,3,4,5,6},
                                 Node.toIntArray(
                                     sort(
                                         ListShuffle.shuffle(
                                             Node.fromIntArray(
                                                 new int[] {1,2,3,4,5,6})))));
        System.out.println("OK");
    }
}
