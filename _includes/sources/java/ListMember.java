/*
public class Node<T> {
    T item;
    Node<T> next;
}
*/

public class ListMember {
    public static <T extends Comparable<T>>boolean isMember(T key, Node<T> node) {
        //+BEGIN_SOLUTION
        for (Node<T> n = node; n != null; n = n.next) {
            if (n.item.compareTo(key) == 0)
                return true;
        }
        return false;
        //+END_SOLUTION
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertFalse(isMember(1, null));

        Node<Integer> n = Node.fromIntArray(new int[] {1,2,3});
        Assert.assertTrue(isMember(1, n));
        Assert.assertTrue(isMember(2, n));
        Assert.assertTrue(isMember(3, n));
        Assert.assertFalse(isMember(4, n));

        System.out.println("OK");
    }
}
