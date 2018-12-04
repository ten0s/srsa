import java.util.NoSuchElementException;

public class Main {
    static class ListStack<Item> {

        public ListStack() {}

        public boolean isEmpty() {

        }

        public int size() {

        }

        public void push(Item item) {

        }

        public Item pop() {

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
