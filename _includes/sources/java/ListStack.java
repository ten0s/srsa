import java.util.NoSuchElementException;

public class ListStack<Item> {
    // BEGIN_SOLUTION
    private class Node {
        Item item;
        Node next;
    }

    private Node first;
    private int size;
    // END_SOLUTION

    public boolean isEmpty() {
        // BEGIN_SOLUTION
        return size == 0;
        // END_SOLUTION
    }

    public int size() {
        // BEGIN_SOLUTION
        return size;
        // END_SOLUTION
    }

    public void push(Item item) {
        // BEGIN_SOLUTION
        Node oldfirst = first;
        first = new Node();
        first.item = item;
        first.next = oldfirst;
        size++;
        // END_SOLUTION
    }

    public Item pop() {
        // BEGIN_SOLUTION
        if (first == null) {
            throw new NoSuchElementException();
        }

        Item item = first.item;
        first = first.next;
        size--;
        return item;
        // END_SOLUTION
    }

    public static void main(String[] args) throws Throwable {
        ListStack<String> s = new ListStack<>();
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
