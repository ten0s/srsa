public class BinTree {
    public static final boolean RED   = true;
    public static final boolean BLACK = false;

    public static class Node<T> {
        public T item;
        public Node<T> left;
        public Node<T> right;
        public int size;
        public boolean color;

        public Node(T item) {
            this.item = item;
            this.size = 1;
        }

        public Node(T item, boolean color) {
            this.item = item;
            this.size = 1;
            this.color = color;
        }
    }

    public static <T> void println(Node<T> n) {
        println(0, n);
    }

    private static <T> void println(int offset, Node<T> n) {
        if (n == null) return;
        println(offset+1, n.right);
        for (int i = 0; i < offset; i++) System.out.print("  ");
        System.out.println(n.item);
        println(offset+1, n.left);
    }

    public static int size(Node<?> n) {
        if (n == null) return 0;
        return n.size;
    }

    public static int height(Node<?> n) {
        if (n == null) return 0;
        return 1 + Math.max(height(n.left), height(n.right));
    }

    public static <T> Node<T> fromArray(T[] a) {
        return fromArray(a, 0, a.length-1);
    }

    private static <T> Node<T> fromArray(T[] a, int lo, int hi) {
        if (lo > hi) return null;
        int mid = lo + (hi - lo) / 2;
        Node<T> root = new Node<>(a[mid]);
        root.left  = fromArray(a, lo, mid-1);
        root.right = fromArray(a, mid+1, hi);
        root.size = 1 + size(root.left) + size(root.right);
        return root;
    }

    public static Node<Integer> fromIntArray(int[] a) {
        Integer[] b = new Integer[a.length];
        for (int i = 0; i < a.length; i++) {
            b[i] = a[i];
        }
        return fromArray(b);
    }

    @SuppressWarnings("unchecked")
    public static <T> T[] toArray(Node<T> n) {
        ListQueue<T> q = new ListQueue<>();
        inOrder(q, n);
        T[] a = (T[]) new Object[q.size()];
        for (int i = 0; i < a.length; i++) {
            a[i] = q.dequeue();
        }
        return a;
    }

    private static <T> void inOrder(ListQueue<T> q, Node<T> n) {
        if (n == null) return;
        inOrder(q, n.left);
        q.enqueue(n.item);
        inOrder(q, n.right);
    }

    public static int[] toIntArray(Node<Integer> n) {
        Object[] a = toArray(n);
        int[] b = new int[a.length];
        for (int i = 0; i < a.length; i++) {
            b[i] = (int)a[i];
        }
        return b;
    }

    public static void main(String[] args) throws Throwable {
        int[] a = new int[] {0,1,2,3,4,5,6,7,8,9};
        Assert.assertArrayEquals(a, toIntArray(fromIntArray(a)));

        Assert.assertEquals(0, size(null));
        Assert.assertEquals(10, size(fromIntArray(a)));
        Assert.assertEquals(10, fromIntArray(a).size);
    }
}
