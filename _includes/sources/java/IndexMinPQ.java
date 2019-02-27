public class IndexMinPQ<Key extends Comparable<Key>> extends IndexPQ<Key> {
    public IndexMinPQ(int maxN) { super(maxN); }

    boolean less(Key v, Key w) {
        return v.compareTo(w) > 0;
    }

    public static void main(String[] args) {
        IndexMinPQ<String> pq = new IndexMinPQ<>(7);
        pq.add(1, "1");
        pq.add(3, "3");
        pq.add(5, "5");
        pq.add(2, "2");
        pq.add(4, "4");
        pq.add(6, "6");
        pq.add(0, "0");
        while (!pq.isEmpty()) {
            System.out.println(pq.remove());
        }
    }
}
