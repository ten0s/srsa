import java.util.*;

/*
public class Node<T> {
    T item;
    Node<T> next;
}
*/

class ListsMerge {
    public static <T> Node<T> merge(Node<T> left, Node<T> right) {
        //+BEGIN_SOLUTION
        if (left == null) return right;
        if (right == null) return left;
        Node<T> tmp = left.next;
        left.next = right;
        right.next = merge(tmp, right.next);
        return left;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        int[] a = new int[] {1,3,5,7,9};
        int[] b = new int[] {2,4,6,8,10};
        int[] c = new int[] {1,2,3,4,5,6,7,8,9,10};
        Node<Integer> la = Node.fromIntArray(a);
        Node<Integer> lb = Node.fromIntArray(b);
        Assert.assertArrayEquals(a, Node.toIntArray(merge(la, null)));
        Assert.assertArrayEquals(b, Node.toIntArray(merge(null, lb)));
        Assert.assertArrayEquals(c, Node.toIntArray(merge(la, lb)));
        System.out.println("OK");
    }
    //+END_FOLD }
}
