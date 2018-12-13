import java.util.Arrays;

/*
public class Node<T> {
    T item;
    Node<T> next;

    public static <T> int length(Node<T> node);
    public static <T> Pair<Node<T>, Node<T>> split(int size, Node<T> node);
}

public class Pair<A, B> {
    public A first;
    public B second;
}
*/

class ShuffleList {
    public static <T> Node<T> shuffle(Node<T> node) {
        // SOLUTION_BEGIN
        int length = Node.length(node);
        if (length == 0 || length == 1)
            return node;
        Pair<Node<T>, Node<T>> sub = Node.split(length % 2, node);
        Node<T> left = shuffle(sub.first);
        Node<T> right = shuffle(sub.second);
        return merge(left, right);
        // SOLUTION_END
    }

    // SOLUTION_BEGIN
    private static <T> Node<T> merge(Node<T> left, Node<T> right) {
        if (left == null)
            return right;
        if (right == null)
            return left;
        if (Math.random() < 0.5) {
            left.next = merge(left.next, right);
            return left;
        } else {
            right.next = merge(left, right.next);
            return right;
        }
    }
    // SOLUTION_END

    public static void main(String[] args) throws Throwable {
        for (int i = 0; i < 100; i++) {
            Assert.assertFalse(
                Arrays.equals(
                    new int[] {1,2,3,4,5,6,7,8,9,10},
                    Node.toIntArray(
                        shuffle(
                            Node.fromIntArray(
                                new int[] {1,2,3,4,5,6,7,8,9,10})))));
        }
        System.out.println("OK");
    }
}
