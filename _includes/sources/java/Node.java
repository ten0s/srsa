public class Node<T> {
    public T item;
    public Node<T> next;

    public static <T> int length(Node<T> node) {
        int length = 0;
        for (Node<T> n = node; n != null; n = n.next)
            length++;
        return length;
    }

    public static <T> Pair<Node<T>, Node<T>> split(Node<T> node) {
        if (node == null) {
            throw new IllegalArgumentException("List is empty");
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
    }

    public static <T> Pair<Node<T>, Node<T>> split(int size, Node<T> node) {
        if (node == null) {
            throw new IllegalArgumentException("List is empty");
        }
        Node<T> n = node;
        for (int i = 1; i < size && n != null; i++) {
            n = n.next;
        }
        if (n == null) {
            throw new IllegalArgumentException("List is too short");
        }
        Pair<Node<T>, Node<T>> subs = new Pair<>();
        subs.first = node;
        subs.second = n.next;
        n.next = null;
        return subs;
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

    public static <T> Node<T> fromArray(T[] a) {
        Node<T> prev = null;
        for (int i = a.length-1; i >= 0; i--) {
            Node<T> node = new Node<>();
            node.item = a[i];
            node.next = prev;
            prev = node;
        }
        return prev;
    }

    @SuppressWarnings("unchecked")
    public static <T> T[] toArray(Node<T> node) {
        T[] a = (T[]) new Object[length(node)];
        int i = 0;
        for (Node<T> n = node; n != null; n = n.next) {
			a[i++] = n.item;
		}
        return a;
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertEquals(0, length(fromIntArray(new int[] {})));
        Assert.assertEquals(3, length(fromIntArray(new int[] {1,2,3})));

        Pair<Node<Integer>, Node<Integer>> subs = new Pair<>();

        try {
            split(null);
            Assert.assertTrue(false);
        } catch (IllegalArgumentException e) {}

        subs = split(Node.fromIntArray(new int[] {1}));
        Assert.assertArrayEquals(new int[] {1}, Node.toIntArray(subs.first));
        Assert.assertNull(subs.second);

        subs = split(Node.fromIntArray(new int[] {1,2}));
        Assert.assertArrayEquals(new int[] {1}, Node.toIntArray(subs.first));
        Assert.assertArrayEquals(new int[] {2}, Node.toIntArray(subs.second));

        subs = split(Node.fromIntArray(new int[] {1,2,3}));
        Assert.assertArrayEquals(new int[] {1,2}, Node.toIntArray(subs.first));
        Assert.assertArrayEquals(new int[] {3}, Node.toIntArray(subs.second));

        try {
            split(1, null);
            Assert.assertTrue(false);
        } catch (IllegalArgumentException e) {}

        try {
            split(4, Node.fromIntArray(new int[] {1,2,3}));
            Assert.assertTrue(false);
        } catch (IllegalArgumentException e) {}

        subs = split(3, Node.fromIntArray(new int[] {1,2,3}));
        Assert.assertArrayEquals(new int[] {1,2,3}, Node.toIntArray(subs.first));
        Assert.assertNull(subs.second);

        subs = split(2, Node.fromIntArray(new int[] {1,2,3,4}));
        Assert.assertArrayEquals(new int[] {1,2}, Node.toIntArray(subs.first));
        Assert.assertArrayEquals(new int[] {3,4}, Node.toIntArray(subs.second));
    }
}
