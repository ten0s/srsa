import java.util.NoSuchElementException;

public class Node<T> {
    public T item;
    public Node<T> next;

    public Node(T item) {
        this.item = item;
    }

    public Node(T item, Node<T> next) {
        this.item = item;
        this.next = next;
    }

    public static <T> Node<T> empty() {
        return null;
    }

    public static <T> Node<T> cons(T item, Node<T> list) {
        return new Node<>(item, list);
    }

    public static <T> T head(Node<T> list) {
        if (isEmpty(list)) {
            throw new NoSuchElementException();
        }
        return list.item;
    }

    public static <T> Node<T> tail(Node<T> list) {
        if (isEmpty(list)) {
            throw new NoSuchElementException();
        }
        return list.next;
    }

    public static <T> boolean isEmpty(Node<T> list) {
        return list == empty();
    }

    public static <T> int length(Node<T> node) {
        int length = 0;
        for (Node<T> n = node; n != null; n = n.next)
            length++;
        return length;
    }

    // assumes no cycle
    public static <T> Node<T> last(Node<T> node) {
        if (isEmpty(node)) {
            throw new NoSuchElementException();
        }
        for (; node.next != null; node = node.next) {}
        return node;
    }

    public static <T extends Comparable<T>> T max(Node<T> list) {
        if (isEmpty(list)) {
            throw new NoSuchElementException();
        }
        T me = head(list);
        Node<T> tail = tail(list);
        if (isEmpty(tail)) {
            return me;
        }
        T they = max(tail);
        if (me.compareTo(they) >= 0) {
            return me;
        } else {
            return they;
        }
    }

    public static <T> void println(Node<T> node) {
        ArrayUtil.println(toArray(node));
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

    public static <T> Node<T> fromArray(T[] a) {
        Node<T> prev = null;
        for (int i = a.length-1; i >= 0; i--) {
            prev = new Node<>(a[i], prev);
        }
        return prev;
    }

    public static Node<Integer> fromIntArray(int[] a) {
        Integer[] b = new Integer[a.length];
        for (int i = 0; i < a.length; i++) {
            b[i] = a[i];
        }
        return fromArray(b);
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

    public static int[] toIntArray(Node<Integer> node) {
        Object[] a = toArray(node);
        int[] b = new int[a.length];
        for (int i = 0; i < a.length; i++) {
            b[i] = (int)a[i];
        }
        return b;
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertEquals(0, length(fromIntArray(new int[] {})));
        Assert.assertEquals(3, length(fromIntArray(new int[] {1,2,3})));

        try {
            last(null);
            Assert.assertTrue(false);
        } catch (NoSuchElementException e) {}
        Assert.assertEquals(1, (int)last(fromIntArray(new int[] {1})).item);
        Assert.assertEquals(3, (int)last(fromIntArray(new int[] {1,2,3})).item);

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
