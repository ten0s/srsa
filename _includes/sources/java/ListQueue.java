import java.util.NoSuchElementException;

public class ListQueue<Item> {
    //+BEGIN_SOLUTION
    private class Node {
        Item item;
        Node next;
    }

    private Node first;
    private Node last;
    private int size;
    //+END_SOLUTION

    public boolean isEmpty() {
        //+BEGIN_SOLUTION
        return size == 0;
        //+END_SOLUTION
    }

    public int size() {
        //+BEGIN_SOLUTION
        return size;
        //+END_SOLUTION
    }

    public void enqueue(Item item) {
        //+BEGIN_SOLUTION
        Node oldlast = last;
        last = new Node();
        last.item = item;
        if (oldlast == null) {
            first = last;
        } else {
            oldlast.next = last;
        }
        size++;
        //+END_SOLUTION
    }

    public Item dequeue() {
        //+BEGIN_SOLUTION
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
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
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
    //+END_FOLD }
}
