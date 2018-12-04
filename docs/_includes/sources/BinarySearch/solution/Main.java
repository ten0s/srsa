public class Main {
    // find index of key in a sorted array
    public static int indexOf(int key, int[] a) {
        int lo = 0;
        int hi = a.length - 1;
        while (lo <= hi) {
            // Key is in a[lo..hi] or not present.
            int mid = lo + (hi - lo) / 2;
            if      (key < a[mid]) hi = mid - 1;
            else if (key > a[mid]) lo = mid + 1;
            else return mid;
        }
        return -1;
    }

    public static void main(String[] args) {
        try {
            assert -1 == indexOf(0, new int[] {});
            assert 0 == indexOf(1, new int[] {1,2,3,4,5,6,7,8,9,10});
            assert 9 == indexOf(10, new int[] {1,2,3,4,5,6,7,8,9,10});
            assert 5 == indexOf(6, new int[] {1,2,3,4,5,6,7,8,9,10});
            System.out.println("OK");
        } catch (Throwable e) {
            System.out.println("FAIL");
            e.printStackTrace();
        }
    }
}
