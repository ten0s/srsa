public class ArrayRank {
    public static int rank(int key, int[] a) {
        // BEGIN_SOLUTION
        // * if key is in the table, returns its index, which is the same as
        // the number of keys that are smaller than key
        // * if key is not in the table, returns the number of keys that are
        // smaller than key
        int lo = 0, hi = a.length - 1;
        while (lo <= hi) {
            int mid = lo + (hi - lo) / 2;
            if      (key < a[mid]) hi = mid - 1;
            else if (key > a[mid]) lo = mid + 1;
            else return mid;
        }
        return lo;
        // END_SOLUTION
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertEquals(0, rank(-1, new int[] {}));
        Assert.assertEquals(0, rank(0, new int[] {}));
        Assert.assertEquals(0, rank(1, new int[] {}));

        Assert.assertEquals(0, rank(-1, new int[] {1,2,3,4,5,6,7,8,9,10}));
        Assert.assertEquals(0, rank(0, new int[] {1,2,3,4,5,6,7,8,9,10}));
        Assert.assertEquals(0, rank(1, new int[] {1,2,3,4,5,6,7,8,9,10}));

        Assert.assertEquals(5, rank(6, new int[] {1,2,3,4,5,6,7,8,9,10}));
        Assert.assertEquals(9, rank(10, new int[] {1,2,3,4,5,6,7,8,9,10}));
        Assert.assertEquals(10, rank(11, new int[] {1,2,3,4,5,6,7,8,9,10}));

        Assert.assertEquals(3, rank(5, new int[] {1,3,4,6,7,8,9}));
        Assert.assertEquals(7, rank(10, new int[] {1,3,4,6,7,8,9}));
        System.out.println("OK");
    }
}
