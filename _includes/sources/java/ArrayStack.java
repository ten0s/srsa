import java.util.NoSuchElementException;

public class ArrayStack<Item> {
    private final int MIN_CAPACITY = 2;
    private Item[] a;
    private int n;

    @SuppressWarnings("unchecked")
    public ArrayStack() {
        a = (Item[]) new Object[MIN_CAPACITY];
    }

    public void push(Item item) {
        // BEGIN_SOLUTION
        if (n == a.length) resize(2 * a.length);
        a[n++] = item;
        // END_SOLUTION
    }

    public Item pop() {
        // BEGIN_SOLUTION
        if (isEmpty()) {
            throw new NoSuchElementException();
        }

        Item item = a[--n];
        a[n] = null;
        if (n == a.length/4) {
            resize(Math.max(MIN_CAPACITY, a.length/2));
        }
        return item;
        // END_SOLUTION
    }

    public boolean isEmpty() {
        // BEGIN_SOLUTION
        return size() == 0;
        // END_SOLUTION
    }

    public int size() {
        // BEGIN_SOLUTION
        return n;
        // END_SOLUTION
    }

    @SuppressWarnings("unchecked")
    private void resize(int capacity) {
        Item[] b = (Item[]) new Object[capacity];
        for (int i = 0; i < n; i++)
            b[i] = a[i];
        a = b;
    }

    private int capacity() {
        return a.length;
    }

    public static void main(String[] args) throws Throwable {
        ArrayStack<String> s = new ArrayStack<>();
        Assert.assertTrue(s.isEmpty());
        Assert.assertEquals(0, s.size());
        Assert.assertEquals(s.MIN_CAPACITY, s.capacity());

        s.push("1");
        s.push("2");
        s.push("3");
        s.push("4");
        s.push("5");
        Assert.assertFalse(s.isEmpty());
        Assert.assertEquals(5, s.size());
        Assert.assertEquals(8, s.capacity());

        Assert.assertEquals("5", s.pop());
        Assert.assertEquals("4", s.pop());
        Assert.assertEquals("3", s.pop());
        Assert.assertEquals(4, s.capacity());

        Assert.assertEquals("2", s.pop());
        Assert.assertEquals("1", s.pop());
        Assert.assertTrue(s.isEmpty());
        Assert.assertEquals(0, s.size());
        Assert.assertEquals(s.MIN_CAPACITY, s.capacity());
        try {
            s.pop();
            Assert.assertTrue(false);
        } catch (NoSuchElementException e) {}
        System.out.println("OK");
    }
}
