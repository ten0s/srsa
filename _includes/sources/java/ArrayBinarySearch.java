public class ArrayBinarySearch {
    public static int indexOf(int key, int[] a) {
        // BEGIN_SOLUTION
        int lo = 0, hi = a.length - 1;
        while (lo <= hi) {
            // Key is in a[lo..hi] or not present.
            int mid = lo + (hi - lo) / 2;
            if      (key < a[mid]) hi = mid - 1;
            else if (key > a[mid]) lo = mid + 1;
            else return mid;
        }
        return -1;
        // END_SOLUTION
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertEquals(-1, indexOf(0, new int[] {}));
        Assert.assertEquals(0, indexOf(1, new int[] {1,2,3,4,5,6,7,8,9,10}));
        Assert.assertEquals(9, indexOf(10, new int[] {1,2,3,4,5,6,7,8,9,10}));
        Assert.assertEquals(5, indexOf(6, new int[] {1,2,3,4,5,6,7,8,9,10}));
        Assert.assertEquals(-1, indexOf(0, new int[] {1,2,3,4,5,6,7,8,9,10}));
        Assert.assertEquals(-1, indexOf(11, new int[] {1,2,3,4,5,6,7,8,9,10}));
        System.out.println("OK");
    }
}
