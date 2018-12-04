import java.util.NoSuchElementException;

/*
public class Node<T> {
    T item;
    Node<T> next;
}
*/

public class Main {
    static class ListQueue<Item> {
        // SOLUTION_BEGIN
        Node<Item> first;
        Node<Item> last;
        int size;
        // SOLUTION_END

        public ListQueue() {}

        public boolean isEmpty() {
            // SOLUTION_BEGIN
            return size == 0;
            // SOLUTION_END
        }

        public int size() {
            // SOLUTION_BEGIN
            return size;
            // SOLUTION_END
        }

        public void enqueue(Item item) {
            // SOLUTION_BEGIN
            Node<Item> oldlast = last;
            last = new Node<Item>();
            last.item = item;
            if (oldlast == null) {
                first = last;
            } else {
                oldlast.next = last;
            }
            size++;
            // SOLUTION_END
        }

        public Item dequeue() {
            // SOLUTION_BEGIN
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
            // SOLUTION_END
        }
    }

    public static void main(String[] args) throws Throwable {
        ListQueue<String> q = new ListQueue<>();
        Assert.assertTrue(q.isEmpty());
        Assert.assertEquals(0, q.size());
        q.enqueue("1");
        q.enqueue("2");
        Assert.assertFalse(q.isEmpty());
        Assert.assertEquals(2, q.size());
        Assert.assertEquals("1", q.dequeue());
        q.enqueue("3");
        Assert.assertEquals("2", q.dequeue());
        Assert.assertEquals("3", q.dequeue());
        Assert.assertTrue(q.isEmpty());
        Assert.assertEquals(0, q.size());
        try {
            q.dequeue();
            Assert.assertTrue(false);
        } catch (NoSuchElementException e) {}
        System.out.println("OK");
    }
}
