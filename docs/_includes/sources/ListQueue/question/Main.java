import java.util.NoSuchElementException;

public class Main {
    static class ListQueue<Item> {

        public ListQueue() {}

        public boolean isEmpty() {

        }

        public int size() {

        }

        public void enqueue(Item item) {

        }

        public Item dequeue() {

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
