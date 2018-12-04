import java.util.NoSuchElementException;

/*
public class Node<T> {
    T item;
    Node<T> next;
}
*/

public class Main {
    static class ListStack<Item> {
        // SOLUTION_BEGIN
        Node<Item> first;
        int size;
        // SOLUTION_END

        public ListStack() {}

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

        public void push(Item item) {
            // SOLUTION_BEGIN
            Node<Item> oldfirst = first;
            first = new Node<Item>();
            first.item = item;
            first.next = oldfirst;
            size++;
            // SOLUTION_END
        }

        public Item pop() {
            // SOLUTION_BEGIN
            if (first == null) {
                throw new NoSuchElementException();
            }

            Item item = first.item;
            first = first.next;
            size--;
            return item;
            // SOLUTION_END
        }
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
