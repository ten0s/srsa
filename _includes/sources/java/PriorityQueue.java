import java.util.NoSuchElementException;

public class PriorityQueue {
    static abstract class PQ<Key extends Comparable<Key>> {
        protected final int MIN_CAPACITY = 2;
        private Key[] pq;
        private int n;

        @SuppressWarnings("unchecked")
        private PQ() {
            pq = (Key[]) new Comparable[MIN_CAPACITY+1];
        }

        protected void push(Key v) {
            // SOLUTION_BEGIN
            n++;
            if (n == capacity()) resize(2*capacity()+1);
            pq[n] = v;
            swim(n);
            // SOLUTION_END
        }

        protected Key pop() {
            // SOLUTION_BEGIN
            checkEmpty();
            Key v = pq[1];
            pq[1] = pq[n];
            pq[n] = null;
            if (n == capacity()/4) resize(Math.max(MIN_CAPACITY, capacity()/2)+1);
            n--;
            sink(1);
            return v;
            // SOLUTION_END
        }

        protected Key peek() {
            // SOLUTION_BEGIN
            checkEmpty();
            return pq[1];
            // SOLUTION_END
        }

        public boolean isEmpty() {
            return size() == 0;
        }

        public int size() {
            return n;
        }

        // SOLUTION_BEGIN
        private void swim(int k) {
            while (k > 1 && less(k/2, k)) {
                swap(k/2, k);
                k = k/2;
            }
        }

        private void sink(int k) {
            while (2*k <= n) {
                int i = 2*k;
                if (i < n && less(i, i+1)) ++i;
                if (!less(k, i)) break;
                swap(k, i);
                k = i;
            }
        }
        // SOLUTION_END

        private boolean less(int i, int j) {
            return less(pq[i], pq[j]);
        }

        abstract boolean less(Key v, Key w);

        private void swap(int i, int j) {
            Key tmp = pq[i]; pq[i] = pq[j]; pq[j] = tmp;
        }

        @SuppressWarnings("unchecked")
        private void resize(int capacity) {
            Key[] pq2 = (Key[]) new Comparable[capacity];
            for (int i = 1; i < n; i++) {
                pq2[i] = pq[i];
            }
            pq = pq2;
        }

        protected int capacity() {
            return pq.length - 1;
        }

        private void checkEmpty() {
            if (isEmpty()) {
                throw new NoSuchElementException();
            }
        }
    }

    private static class PQMax<Key extends Comparable<Key>> extends PQ<Key> {
        public void insert(Key v) { push(v); }
        public Key delMax() { return pop(); }
        public Key max() { return peek(); }

        // SOLUTION_BEGIN
        boolean less(Key v, Key w) {
            return v.compareTo(w) < 0;
        }
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        PQMax<Integer> pq = new PQMax<>();
        Assert.assertTrue(pq.isEmpty());
        Assert.assertEquals(0, pq.size());
        Assert.assertEquals(pq.MIN_CAPACITY, pq.capacity());

        pq.insert(1);
        pq.insert(3);
        pq.insert(5);
        pq.insert(7);
        pq.insert(9);
        pq.insert(2);
        pq.insert(4);
        pq.insert(6);
        pq.insert(8);
        pq.insert(10);

        Assert.assertFalse(pq.isEmpty());
        Assert.assertEquals(10, pq.size());
        Assert.assertEquals(16, pq.capacity());

        Assert.assertEquals(10, (int)pq.max());
        Assert.assertEquals(10, (int)pq.delMax());
        for (int i = 0; i < 8; i++) pq.delMax();
        Assert.assertEquals(1, (int)pq.max());
        Assert.assertEquals(1, (int)pq.delMax());

        Assert.assertTrue(pq.isEmpty());
        Assert.assertEquals(0, pq.size());
        Assert.assertEquals(pq.MIN_CAPACITY, pq.capacity());

        try {
            pq.max();
            Assert.assertTrue(false);
        } catch (NoSuchElementException e) {}

        try {
            pq.delMax();
            Assert.assertTrue(false);
        } catch (NoSuchElementException e) {}

        System.out.println("OK");

    }
}
