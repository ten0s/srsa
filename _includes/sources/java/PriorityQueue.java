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

        protected void insert(Key v) {
            // SOLUTION_BEGIN
            if (++n > capacity()) resize(2*capacity()+1);
            pq[n] = v;
            swim(n);
            // SOLUTION_END
        }

        protected Key delete() {
            // SOLUTION_BEGIN
            ensureNotEmpty();
            Key v = pq[1];
            pq[1] = pq[n];
            pq[n] = null;
            if (--n <= capacity()/4) resize(Math.max(MIN_CAPACITY, capacity()/2)+1);
            sink(1);
            return v;
            // SOLUTION_END
        }

        protected Key top() {
            // SOLUTION_BEGIN
            ensureNotEmpty();
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
            // while i'm not at root and parent is less than me
            // swap me with parent and move up
            while (k > 1 && less(k/2, k)) {
                swap(k/2, k);
                k = k/2;
            }
        }

        private void sink(int k) {
            // while left child is in the tree
            while (2*k <= n) {
                int j = 2*k;
                // if right child is in the tree, choose max child
                if (j < n && less(j, j+1)) ++j;
                // stop, if i'm not less than child
                if (!less(k, j)) break;
                // swap me with child and move down
                swap(k, j);
                k = j;
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
            int m = Math.min(n, pq.length-1);
            for (int i = 1; i <= m; i++) {
                pq2[i] = pq[i];
            }
            pq = pq2;
        }

        protected int capacity() {
            return pq.length - 1;
        }

        private void ensureNotEmpty() {
            if (isEmpty()) {
                throw new NoSuchElementException();
            }
        }
    }

    private static class MaxPQ<Key extends Comparable<Key>> extends PQ<Key> {
        public Key delMax() { return delete(); }
        public Key max() { return top(); }

        // SOLUTION_BEGIN
        boolean less(Key v, Key w) {
            return v.compareTo(w) < 0;
        }
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        MaxPQ<Integer> pq = new MaxPQ<>();
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

        Assert.assertEquals(10, pq.max());
        Assert.assertEquals(10, pq.delMax());
        for (int i = 0; i < 8; i++) pq.delMax();
        Assert.assertEquals(1, pq.max());
        Assert.assertEquals(1, pq.delMax());

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
