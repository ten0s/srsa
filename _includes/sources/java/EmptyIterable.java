//+BEGIN_SOLUTION
import java.util.Iterator;
import java.util.Collections;
//+END_SOLUTION

public class EmptyIterable {
    static class Test<Item> {
        public Iterable<Item> items() {
            //+BEGIN_SOLUTION
            return new Iterable<Item>() {
                public Iterator<Item> iterator() {
                    return Collections.emptyIterator();
                }
            };
            //+END_SOLUTION
        }
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Test<Integer> t = new Test<>();
        for (int x : t.items()) {}
        System.out.println("OK");
    }
    //+END_FOLD }
}
