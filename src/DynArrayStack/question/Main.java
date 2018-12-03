import java.util.NoSuchElementException;

public class Main {
    static class ArrayStack<Item> {
        private Item[] a;

        @SuppressWarnings("unchecked")
        public ArrayStack() {
            a = (Item[]) new Object[2];
        }

        public void push(Item item) {

        }

        public Item pop() {

        }

        public boolean isEmpty() {

        }

        public int size() {

        }

        @SuppressWarnings("unchecked")
        private void resize(int capacity) {
            Item[] b = (Item[]) new Object[capacity];
            for (int i = 0; i < n; i++)
                b[i] = a[i];
            a = b;
        }
    }

    public static void main(String[] args) {
        try {
            ArrayStack<String> s = new ArrayStack<>();
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
