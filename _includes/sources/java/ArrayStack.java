import java.util.NoSuchElementException;

public class ArrayStack<Item> {
    private Item[] a;
    private int n;

    @SuppressWarnings("unchecked")
    public ArrayStack() {
        a = (Item[]) new Object[2];
    }

    public void push(Item item) {
        // SOLUTION_BEGIN
        if (n == a.length) resize(2 * a.length);
        a[n++] = item;
        // SOLUTION_END
    }

    public Item pop() {
        // SOLUTION_BEGIN
        if (isEmpty()) {
            throw new NoSuchElementException();
        }

        if (n <= a.length / 4) resize(a.length / 2);
        Item item = a[--n];
        a[n] = null;
        return item;
        // SOLUTION_END
    }

    public boolean isEmpty() {
        // SOLUTION_BEGIN
        return size() == 0;
        // SOLUTION_END
    }

    public int size() {
        // SOLUTION_BEGIN
        return n;
        // SOLUTION_END
    }

    @SuppressWarnings("unchecked")
    private void resize(int capacity) {
        Item[] b = (Item[]) new Object[capacity];
        for (int i = 0; i < n; i++)
            b[i] = a[i];
        a = b;
    }

    public static void main(String[] args) throws Throwable {
        ArrayStack<String> s = new ArrayStack<>();
        Assert.assertTrue(s.isEmpty());
        Assert.assertEquals(0, s.size());
        s.push("1");
        s.push("2");
        s.push("3");
        Assert.assertFalse(s.isEmpty());
        Assert.assertEquals(3, s.size());
        Assert.assertEquals("3", s.pop());
        Assert.assertEquals("2", s.pop());
        Assert.assertEquals("1", s.pop());
        Assert.assertTrue(s.isEmpty());
        Assert.assertEquals(0, s.size());
        try {
            s.pop();
            Assert.assertTrue(false);
        } catch (NoSuchElementException e) {}
        System.out.println("OK");
    }
}
