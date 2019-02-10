import java.util.NoSuchElementException;

public class ArrayQueue<Item> {
    private final int MIN_CAPACITY = 2;
    private Item[] a;
    private int head;
    private int tail;

    @SuppressWarnings("unchecked")
    public ArrayQueue() {
        a = (Item[]) new Object[MIN_CAPACITY];
    }

    public void enqueue(Item item) {
        //+BEGIN_SOLUTION
        // if tail is at the end
        if (tail == a.length) {
            // if there's some place in the front
            if (head > 0)
                relocate();
            else
                resize(2*a.length);
        }
        a[tail++] = item;
        //+END_SOLUTION
    }

    public Item dequeue() {
        //+BEGIN_SOLUTION
        if (isEmpty()) {
            throw new NoSuchElementException();
        }
        Item item = a[head];
        a[head++] = null;
        if (size() <= a.length/4) {
            resize(Math.max(MIN_CAPACITY, a.length/2));
        }
        return item;
        //+END_SOLUTION
    }

    public boolean isEmpty() {
        //+BEGIN_SOLUTION
        return size() == 0;
        //+END_SOLUTION
    }

    public int size() {
        //+BEGIN_SOLUTION
        return tail - head;
        //+END_SOLUTION
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

    private int capacity() {
        return a.length;
    }

    public static void main(String[] args) throws Throwable {
        ArrayQueue<String> q = new ArrayQueue<>();
        Assert.assertTrue(q.isEmpty());
        Assert.assertEquals(0, q.size());
        Assert.assertEquals(q.MIN_CAPACITY, q.capacity());

        q.enqueue("1");
        q.enqueue("2");
        q.enqueue("3");
        q.enqueue("4");
        Assert.assertFalse(q.isEmpty());
        Assert.assertEquals(4, q.size());
        Assert.assertEquals(4, q.capacity());

        // check relocate
        Assert.assertEquals("1", q.dequeue());
        q.enqueue("5");
        Assert.assertEquals(4, q.size());
        Assert.assertEquals(4, q.capacity());

        // check resize up
        q.enqueue("6");
        Assert.assertEquals(5, q.size());
        Assert.assertEquals(8, q.capacity());

        // check resize down
        Assert.assertEquals("2", q.dequeue());
        Assert.assertEquals("3", q.dequeue());
        Assert.assertEquals("4", q.dequeue());
        Assert.assertEquals(2, q.size());
        Assert.assertEquals(4, q.capacity());

        Assert.assertEquals("5", q.dequeue());
        Assert.assertEquals("6", q.dequeue());
        Assert.assertTrue(q.isEmpty());
        Assert.assertEquals(0, q.size());
        Assert.assertEquals(q.MIN_CAPACITY, q.capacity());

        try {
            q.dequeue();
            Assert.assertTrue(false);
        } catch (NoSuchElementException e) {}
        System.out.println("OK");
    }
}
