public class Node<T> {
    public T item;
    public Node next;

    public static int[] toIntArray(Node node) {
        int size = 0;
        for (Node n = node; n != null; n = n.next) {
            size++;
        }
        int[] a = new int[size];
        int i = 0;
        for (Node n = node; n != null; n = n.next) {
			a[i++] = (int)n.item;
		}
        return a;
    }
}
