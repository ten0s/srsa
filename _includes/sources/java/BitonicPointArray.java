public class BitonicPointArray {
    public static int indexOf(int[] a) {
        // SOLUTION_BEGIN
        return indexOf(a, 0, a.length-1);
        // SOLUTION_END
    }

    // SOLUTION_BEGIN
    private static int indexOf(int[] a, int lo, int hi) {
        if (lo > hi) {
            return -1;
        }

        if (lo == hi) {
            return lo;
        }

        if (hi - lo == 1) {
            int diff = a[lo] - a[hi];
            if (diff < 0) {
                return hi;
            } else if (diff > 0) {
                return lo;
            } else {
                // Some not distinct values
                throw new IllegalArgumentException();
            }
        }

        int mid = lo + (hi - lo) / 2;
        if (a[mid-1] < a[mid] && a[mid] > a[mid+1]) {
            return mid;
        } else if (a[mid-1] < a[mid]) {
            return indexOf(a, mid+1, hi);
        } else {
            return indexOf(a, lo, mid-1);
        }
    }
    // SOLUTION_END

    public static void main(String[] args) throws Throwable {
        Assert.assertEquals(-1, indexOf(new int[] {}));
        Assert.assertEquals(0, indexOf(new int[] {1}));
        Assert.assertEquals(1, indexOf(new int[] {1,2}));
        Assert.assertEquals(0, indexOf(new int[] {2,1}));
        Assert.assertEquals(1, indexOf(new int[] {1,2,1}));
        Assert.assertEquals(2, indexOf(new int[] {1,3,5,4,2}));
        Assert.assertEquals(3, indexOf(new int[] {1,3,5,7,6,4,2}));
        System.out.println("OK");
    }
}
