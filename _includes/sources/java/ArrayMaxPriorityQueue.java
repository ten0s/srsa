import java.util.NoSuchElementException;

public class ArrayMaxPriorityQueue {
    static abstract class PQ<Key extends Comparable<Key>> {
        protected final int MIN_CAPACITY = 2;
        private Key[] pq;
        private int n;

        @SuppressWarnings("unchecked")
        private PQ() {
            pq = (Key[]) new Comparable[MIN_CAPACITY+1];
        }

        public void add(Key v) {
            if (++n > capacity()) resize(2*capacity()+1);
            //+BEGIN_SOLUTION
            pq[n] = v;
            swim(n);
            //+END_SOLUTION
        }

        //+BEGIN_SOLUTION
        private void swim(int k) {
            // while i'm not at root and parent is less than me
            // swap me with parent and move up
            while (k > 1 && less(k/2, k)) {
                swap(k/2, k);
                k = k/2;
            }
        }
        //+END_SOLUTION

        public Key remove() {
            ensureNotEmpty();
            //+BEGIN_SOLUTION
            Key v = pq[1];
            pq[1] = pq[n];
            pq[n] = null;
            //+END_SOLUTION
            if (--n <= capacity()/4) resize(Math.max(MIN_CAPACITY, capacity()/2)+1);
            //+BEGIN_SOLUTION
            sink(1);
            return v;
            //+END_SOLUTION
        }

        //+BEGIN_SOLUTION
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
        //+END_SOLUTION

        public Key peek() {
            ensureNotEmpty();
            //+BEGIN_SOLUTION
            return pq[1];
            //+END_SOLUTION
        }

        public boolean isEmpty() {
            return size() == 0;
        }

        public int size() {
            return n;
        }


        private boolean less(int i, int j) {
            return less(pq[i], pq[j]);
        }

        private void swap(int i, int j) {
            Key tmp = pq[i]; pq[i] = pq[j]; pq[j] = tmp;
        }

        abstract boolean less(Key v, Key w);

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
        //+BEGIN_SOLUTION
        boolean less(Key v, Key w) {
            return v.compareTo(w) < 0;
        }
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        MaxPQ<Integer> pq = new MaxPQ<>();
        Assert.assertTrue(pq.isEmpty());
        Assert.assertEquals(0, pq.size());
        Assert.assertEquals(pq.MIN_CAPACITY, pq.capacity());

        pq.add(1);
        pq.add(3);
        pq.add(5);
        pq.add(7);
        pq.add(9);
        pq.add(2);
        pq.add(4);
        pq.add(6);
        pq.add(8);
        pq.add(10);

        Assert.assertFalse(pq.isEmpty());
        Assert.assertEquals(10, pq.size());
        Assert.assertEquals(16, pq.capacity());

        Assert.assertEquals(10, (int)pq.peek());
        Assert.assertEquals(10, (int)pq.remove());
        for (int i = 0; i < 8; i++) pq.remove();
        Assert.assertEquals(1, (int)pq.peek());
        Assert.assertEquals(1, (int)pq.remove());

        Assert.assertTrue(pq.isEmpty());
        Assert.assertEquals(0, pq.size());
        Assert.assertEquals(pq.MIN_CAPACITY, pq.capacity());

        try {
            pq.peek();
            Assert.assertTrue(false);
        } catch (NoSuchElementException e) {}

        try {
            pq.remove();
            Assert.assertTrue(false);
        } catch (NoSuchElementException e) {}

        System.out.println("OK");
    }
    //+END_FOLD }
}
