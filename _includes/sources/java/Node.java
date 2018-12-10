public class Node<T> {
    public T item;
    public Node<T> next;

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
        int size = 0;
        for (Node n = node; n != null; n = n.next) {
            size++;
        }
        int[] a = new int[size];
        int i = 0;
        for (Node<Integer> n = node; n != null; n = n.next) {
			a[i++] = n.item;
		}
        return a;
    }
}
