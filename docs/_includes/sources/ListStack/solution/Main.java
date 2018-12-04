import java.util.NoSuchElementException;

public class Main {
    static class ListStack<Item> {
        private class Node {
            Item item;
            Node next;
        }

        Node first;
        int size;

        public ListStack() {}

        public boolean isEmpty() {
            return size == 0;
        }

        public int size() {
            return size;
        }

        public void push(Item item) {
            Node oldfirst = first;
            first = new Node();
            first.item = item;
            first.next = oldfirst;
            size++;
        }

        public Item pop() {
            if (first == null) {
                throw new NoSuchElementException();
            }

            Item item = first.item;
            first = first.next;
            size--;
            return item;
        }

    }

    public static void main(String[] args) {
        try {
            ListStack<String> s = new ListStack<>();
            assert s.isEmpty();
            assert 0 == s.size();
            s.push("1");
            s.push("2");
            s.push("3");
            assert !s.isEmpty();
            assert 3 == s.size();
            assert "3".equals(s.pop());
            assert "2".equals(s.pop());
            assert "1".equals(s.pop());
            assert s.isEmpty();
            assert 0 == s.size();
            try {
                s.pop();
                assert false;
            } catch (NoSuchElementException e) {}
            System.out.println("OK");
        } catch (Throwable e) {
            System.out.println("FAIL");
            e.printStackTrace();
        }
    }
}
