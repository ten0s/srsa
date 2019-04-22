import java.util.Iterator;
import java.util.NoSuchElementException;
import java.lang.UnsupportedOperationException;

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

        //+BEGIN_SOLUTION
        public Iterator<Item> iterator() {
            return new ArrayIterator();
        }

        private class ArrayIterator implements Iterator<Item> {
            private int i = 0;

            public boolean hasNext() {
                return i < a.length;
            }

            public Item next() {
                if (hasNext()) {
                    return a[i++];
                } else {
                    throw new NoSuchElementException();
                }
            }

            public void remove() {
                throw new UnsupportedOperationException();
            }
        }
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
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
    //+END_FOLD }
}
