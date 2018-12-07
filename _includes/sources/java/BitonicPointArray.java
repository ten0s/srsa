public class BitonicPointArray {
    public static int indexOf(int[] a) {
        // SOLUTION_BEGIN
        return indexOf(a, 0, a.length-1);
        // SOLUTION_END
    }

    // SOLUTION_BEGIN
    private static int indexOf(int[] a, int lo, int hi) {
        int diff = hi - lo;
        if (diff < 0)
            return -1;
        else if (diff == 0)
            // 1 item
            return lo;
        else if (diff == 1) {
            // 2 items
            if (lo == 0) {
                if (a[lo] < a[lo+1]) return lo+1;
                else return lo;
            } else {
                if (a[hi-1] > a[hi]) return hi-1;
                else return hi;
            }
        } else {
            // 3+ items
            int mid = lo + (hi - lo) / 2;
            if (a[mid] < a[mid+1])
                return indexOf(a, mid+1, hi);
            else if (a[mid-1] > a[mid])
                return indexOf(a, lo, mid-1);
            else
                return mid;
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
