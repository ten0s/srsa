import java.util.Iterator;
import java.util.NoSuchElementException;

public class Bag<Item extends Comparable> implements Iterable<Item> {
    private class Node {
        Item item;
        Node next;
    }

    private Node first;
    private int size;

    public Bag() {}

    public void add(Item item) {
        Node oldfirst = first;
        first = new Node();
        first.item = item;
        first.next = oldfirst;
        size++;
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public int size() {
        return size;
    }

    public boolean isMember(Item key) {
        for (Node x = first; x != null; x = x.next) {
            if (key.equals(x.item)) {
                return true;
            }
        }
        return false;
    }

    public void remove(Item key) {
        if (first == null) {
            return;
        }

        // if key is in the head?
        if (key.equals(first.item)) {
            first = first.next;
            size--;
            return;
        }

        // if key is in the next?
        for (Node x = first; x.next != null; x = x.next) {
            if (key.equals(x.next.item)) {
                x.next = x.next.next;
                size--;
                break;
            }
        }
    }

    @SuppressWarnings("unchecked")
    public Item[] toArray(Item[] a) {
        int i = 0;
        for (Item e : this) {
            a[i++] = e;
        }
        return a;
    }

    public Iterator<Item> iterator() {
        return new ForwardIterator();
    }

    private class ForwardIterator implements Iterator<Item> {
        Node x = first;

        public boolean hasNext() {
            return x != null;
        }

        public Item next() {
            Item item = x.item;
            x = x.next;
            return item;
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }
}
