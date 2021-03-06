import java.util.*;

/*
public class Node<T> {
    T item;
    Node<T> next;

    public static <T> <Node<T> reverse(Node<T> node);
    public static <T> Pair<Node<T>, Node<T>> split(Node<T> node);
}

public class Pair<A, B> {
    public A first;
    public B second;
}
*/

class ListShuffle {
    public static <T> Node<T> shuffle(Node<T> node) {
        //+BEGIN_SOLUTION
        if (node == null || node.next == null) {
            return node;
        }
        Pair<Node<T>, Node<T>> subs = Node.split(node);
        if (Math.random() < 0.5) {
            subs.first = Node.reverse(subs.first);
        }
        if (Math.random() < 0.5) {
            subs.second = Node.reverse(subs.second);
        }
        if (Math.random() < 0.5) {
            return merge(shuffle(subs.first), shuffle(subs.second));
        } else {
            return merge(shuffle(subs.second), shuffle(subs.first));
        }
        //+END_SOLUTION
    }

    private static <T> Node<T> merge(Node<T> left, Node<T> right) {
        if (left == null) return right;
        if (right == null) return left;
        Node<T> tmp = left.next;
        left.next = right;
        right.next = merge(tmp, right.next);
        return left;
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        final int N = 1000;
        final int[] asc  = new int[] {1,2,3,4,5,6,7,8,9,10};
        final int[] desc = new int[] {10,9,8,7,6,5,4,3,2,1};
        int[] sumsAsc  = new int[asc.length];
        int[] sumsDesc = new int[asc.length];
        for (int i = 0; i < N; i++) {
            final int[] shuffledAsc =
                Node.toIntArray(shuffle(Node.fromIntArray(asc)));
            final int[] shuffledDesc =
                Node.toIntArray(shuffle(Node.fromIntArray(desc)));
            for (int j = 0; j < asc.length; j++) {
                sumsAsc[j]  += shuffledAsc[j];
                sumsDesc[j] += shuffledDesc[j];
            }
        }
        System.out.println(ArrayUtil.toString(sumsAsc));
        Histogram.print(sumsAsc);
        System.out.println(ArrayUtil.toString(sumsDesc));
        Histogram.print(sumsDesc);

        double stdDevAsc = Stat.stdDev(sumsAsc);
        Assert.assertTrue("" + stdDevAsc, stdDevAsc < 150.0);
        double stdDevDesc = Stat.stdDev(sumsDesc);
        Assert.assertTrue("" + stdDevDesc, stdDevDesc < 150.0);

        System.out.println("OK");
    }
    //+END_FOLD }
}
