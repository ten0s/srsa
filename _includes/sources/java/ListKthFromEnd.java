/*
public class Node<T> {
    T item;
    Node<T> next;
}
*/

public class ListKthFromEnd {
    public static <T> T kth(int k, Node<T> node) {
        //+BEGIN_SOLUTION
        Node<T> slow = node;
        Node<T> fast = skip(k, node);
        while (fast != null) {
            slow = slow.next;
            fast = fast.next;
        }
        return slow.item;
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    public static <T> Node<T> skip(int k, Node<T> node) {
        while (k > 0 && node != null) {
            k--;
            node = node.next;
        }
        if (k > 0) {
            throw new IllegalArgumentException("Wrong list length");
        }
        return node;
    }
    //+END_SOLUTION

    public static void main(String[] args) throws Throwable {
        Node<Integer> list = Node.fromIntArray(new int[] {1,2,3,4,5,6,7,8,9,10});

        try {
            kth(1, null);
            Assert.assertTrue(false);
        } catch (IllegalArgumentException e) {}

        try {
            kth(11, list);
            Assert.assertTrue(false);
        } catch (IllegalArgumentException e) {}

        Assert.assertEquals(10, kth(1, list));
        Assert.assertEquals(9, kth(2, list));
        Assert.assertEquals(5, kth(6, list));
        Assert.assertEquals(2, kth(9, list));
        Assert.assertEquals(1, kth(10, list));

        System.out.println("OK");
    }
}
