import java.util.NoSuchElementException;

public class Main {
    static class ListQueue<Item> {
        private class Node {
            Item item;
            Node next;
        }

        Node first;
        Node last;
        int size;

        public ListQueue() {}

        public boolean isEmpty() {
            return size == 0;
        }

        public int size() {
            return size;
        }

        public void enqueue(Item item) {
            Node oldlast = last;
            last = new Node();
            last.item = item;
            if (oldlast == null) {
                first = last;
            } else {
                oldlast.next = last;
            }
            size++;
        }

        public Item dequeue() {
            if (first == null) {
                throw new NoSuchElementException();
            }

            Item item = first.item;
            first = first.next;
            if (first == null) {
                last = null;
            }
            size--;
            return item;
        }
    }

    public static void main(String[] args) {
        try {
            ListQueue<String> q = new ListQueue<>();
            assert q.isEmpty();
            assert 0 == q.size();
            q.enqueue("1");
            q.enqueue("2");
            assert !q.isEmpty();
            assert 2 == q.size();
            assert "1".equals(q.dequeue());
            q.enqueue("3");
            assert "2".equals(q.dequeue());
            assert "3".equals(q.dequeue());
            assert q.isEmpty();
            assert 0 == q.size();
            try {
                q.dequeue();
                assert false;
            } catch (NoSuchElementException e) {}
            System.out.println("OK");
        } catch (Throwable e) {
            System.out.println("FAIL");
            e.printStackTrace();
        }
    }
}
