public class Node<T> {
    public T item;
    public Node<T> next;

    public static <T> int length(Node<T> node) {
        int length = 0;
        for (Node<T> n = node; n != null; n = n.next)
            length++;
        return length;
    }

    public static <T> Pair<Node<T>, Node<T>> split(int size, Node<T> node) {
        if (node == null) {
            throw new IllegalArgumentException("Empty list");
        }
        Pair<Node<T>, Node<T>> sub = new Pair<>();
        sub.first = node;
        Node<T> n = node;
        for (int i = 1; i < size && n != null; i++) {
            n = n.next;
        }
        if (n == null) {
            throw new IllegalArgumentException("Wrong list length");
        } else {
            sub.second = n.next;
            n.next = null;
        }
        return sub;
    }

    public static Node<Integer> fromIntArray(int[] a) {
        Node<Integer> prev = null;
        for (int i = a.length-1; i >= 0; i--) {
            Node<Integer> node = new Node<>();
            node.item = a[i];
            node.next = prev;
            prev = node;
        }
        return prev;
    }

    public static int[] toIntArray(Node<Integer> node) {
        int[] a = new int[length(node)];
        int i = 0;
        for (Node<Integer> n = node; n != null; n = n.next) {
			a[i++] = n.item;
		}
        return a;
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertEquals(0, length(fromIntArray(new int[] {})));
        Assert.assertEquals(3, length(fromIntArray(new int[] {1,2,3})));

        Pair<Node<Integer>, Node<Integer>> sub = new Pair<>();

        Node<Integer> list = null;
        try {
            split(1, list);
            Assert.assertTrue(false);
        } catch (IllegalArgumentException e) {}

        list = Node.fromIntArray(new int[] {1,2,3});
        try {
            split(4, list);
            Assert.assertTrue(false);
        } catch (IllegalArgumentException e) {}

        list = Node.fromIntArray(new int[] {1,2,3});
        sub = split(3, list);
        Assert.assertArrayEquals(new int[] {1,2,3}, Node.toIntArray(sub.first));
        Assert.assertNull(sub.second);

        list = Node.fromIntArray(new int[] {1,2,3,4});
        sub = split(2, list);
        Assert.assertArrayEquals(new int[] {1,2}, Node.toIntArray(sub.first));
        Assert.assertArrayEquals(new int[] {3,4}, Node.toIntArray(sub.second));
    }
}
