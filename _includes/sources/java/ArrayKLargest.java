//+BEGIN_SOLUTION
import java.util.PriorityQueue;
//+END_SOLUTION

public class ArrayKLargest {
    public static int[] largest(int k, int[] a) {
        //+BEGIN_SOLUTION
        PriorityQueue<Integer> pq = new PriorityQueue<>();
        for (int i = 0; i < a.length; i++) {
            pq.add(a[i]);
            if (pq.size() > k) {
                pq.remove();
            }
        }
        int[] r = new int[k];
        for (int i = k-1; i >= 0; i--) {
            r[i] = pq.remove();
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
