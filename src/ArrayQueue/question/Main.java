import java.util.NoSuchElementException;

public class Main {
    static class ArrayQueue<Item> {
        private Item[] a;

        @SuppressWarnings("unchecked")
            public ArrayQueue() {
            a = (Item[]) new Object[2];
        }

        public void enqueue(Item item) {

        }

        public Item dequeue() {

        }

        public boolean isEmpty() {

        }

        public int size() {

        }

        private void relocate() {
            for (int i = head; i < tail; i++) {
                a[i-head] = a[i];
                a[i] = null;
            }
            tail -= head;
            head = 0;
        }

        @SuppressWarnings("unchecked")
        private void resize(int capacity) {
            Item[] b = (Item[]) new Object[capacity];
            for (int i = head; i < tail; i++) {
                b[i-head] = a[i];
            }
            tail -= head;
            head = 0;
            a = b;
        }
    }

    public static void main(String[] args) {
        try {
            ArrayQueue<String> q = new ArrayQueue<>();
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
