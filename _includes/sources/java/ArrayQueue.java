import java.util.NoSuchElementException;

public class ArrayQueue<Item> {
    private Item[] a;
    private int head;
    private int tail;

    @SuppressWarnings("unchecked")
    public ArrayQueue() {
        a = (Item[]) new Object[2];
    }

    public void enqueue(Item item) {
        // SOLUTION_BEGIN
        // if tail is at the end
        if (tail == a.length) {
            // if there's some place in the front
            if (head > 0)
                relocate();
            else
                resize(2 * a.length);
        }
        a[tail++] = item;
        // SOLUTION_END
    }

    public Item dequeue() {
        // SOLUTION_BEGIN
        if (head == tail) {
            throw new NoSuchElementException();
        }

        Item item = a[head];
        a[head] = null;

        if (++head == tail)
            head = tail = 0;

        return item;
        // SOLUTION_END
    }

    public boolean isEmpty() {
        // SOLUTION_BEGIN
        return tail == head;
        // SOLUTION_END
    }

    public int size() {
        // SOLUTION_BEGIN
        return tail - head;
        // SOLUTION_END
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

    public static void main(String[] args) throws Throwable {
        ArrayQueue<String> q = new ArrayQueue<>();
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
