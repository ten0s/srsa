public class ArrayKLargest {
    public static int[] largest(int k, int[] a) {
        //+BEGIN_SOLUTION
        MinPQ<Integer> pq = new MinPQ<>();
        for (int i = 0; i < a.length; i++) {
            pq.insert(a[i]);
            if (pq.size() > k) {
                pq.delMin();
            }
        }
        int[] r = new int[pq.size()];
        for (int i = r.length-1; i >= 0; i--) {
            r[i] = pq.delMin();
        }
        return r;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        int[] a = new int[] {1,23,12,9,30,2,50};
        Assert.assertArrayEquals(new int[] {50}, largest(1, a));
        Assert.assertArrayEquals(new int[] {50,30,23}, largest(3, a));
        Assert.assertArrayEquals(new int[] {50,30,23,12,9,2,1}, largest(7, a));
        System.out.println("OK");
    }
    //+END_FOLD }
}

//+BEGIN_FOLD Refs {
/*
class MinPQ<Key extends Comparable<Key>> {
    public MinPQ();
    public void insert(Key v);
    public Key delMin();
    public int size();
    public boolean isEmpty();
}
*/
//+END_FOLD }
