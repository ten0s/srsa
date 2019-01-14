import java.util.Iterator;
import java.util.NoSuchElementException;

public class Stack<Item> implements Iterable<Item> {
    private class Node {
        Item item;
        Node next;
    }

    private Node first;
    private int size;

    public Stack() {}

    public void push(Item item) {
        Node oldfirst = first;
        first = new Node();
        first.item = item;
        first.next = oldfirst;
        size++;
    }

    public Item pop() {
        if (first == null) {
            throw new NoSuchElementException();
        }

        Item item = first.item;
        first = first.next;
        size--;
        return item;
    }

    public Item peek() {
        if (first == null) {
            throw new NoSuchElementException();
        }
        return first.item;
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public int size() {
        return size;
    }

    public Iterator<Item> iterator() {
        return new BackwardIterator();
    }

    private class BackwardIterator implements Iterator<Item> {
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
