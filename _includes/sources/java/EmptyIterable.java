import java.util.Iterator;
import java.util.Collections;

public class EmptyIterable {
    static class Test<Item> {
        public Iterable<Item> items() {
            // SOLUTION_BEGIN
            return new Iterable<Item>() {
                public Iterator<Item> iterator() {
                    return Collections.emptyIterator();
                }
            };
            // SOLUTION_END
        }
    }

    public static void main(String[] args) throws Throwable {
        Test<Integer> t = new Test<>();
        for (int x : t.items()) {}
        System.out.println("OK");
    }
}
