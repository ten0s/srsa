import java.util.Iterator;
import java.util.NoSuchElementException;

public class IteratorTest {
    static class Array<Item> implements Iterable<Item> {
        private Item[] a;

        @SuppressWarnings("unchecked")
        public Array(Item[] a) {
           this.a = (Item[]) new Object[a.length];
           for (int i = 0; i < a.length; i++) {
               this.a[i] = a[i];
           }
        }

        int size() {
            return a.length;
        }

        // SOLUTION_BEGIN
        public Iterator<Item> iterator() {
            return new ArrayIterator();
        }

        private class ArrayIterator implements Iterator<Item> {
            int c = 0;

            public boolean hasNext() {
                return c < a.length;
            }

            public Item next() {
                if (hasNext()) {
                    return a[c++];
                } else {
                    throw new NoSuchElementException();
                }
            }

            public void remove() {
                throw new UnsupportedOperationException();
            }
        }
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        Array<Integer> arr = new Array<>(new Integer[] {1,2,3,4,5});

        // test foreach
        int sum = 0;
        for (int i : arr) { sum += i; }
        Assert.assertEquals(15, sum);

        // test iterator
        Iterator<Integer> it = arr.iterator();
        try {
            it.remove();
            Assert.assertTrue(false);
        } catch (UnsupportedOperationException e) {}
        while (it.hasNext()) it.next();
        try {
            it.next();
            Assert.assertTrue(false);
        } catch (NoSuchElementException e) {}

        System.out.println("OK");
    }
}
