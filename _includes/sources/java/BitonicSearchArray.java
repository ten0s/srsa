// SOLUTION_BEGIN
/*
The simple idea is: as long as middle is less than key at either side of maximum
we still search in bitonic array, but as soon as middle is greater than key we do
binary search at both sides. Draw /\ graph and check.

https://stackoverflow.com/questions/19372930/given-a-bitonic-array-and-element-x-in-the-array-find-the-index-of-x-in-2logn
Assuming the array is first in ascending order and then in descending order:
1. Take the middle of the array
2. Compare the middle element with one of its neighbor
   to see if the max is on the right or on the left
3. Compare the middle element with the desired value
4. If the middle element is smaller than the desired value AND
   the max is on the left side, then do bitonic search on the left
   subarray (we are sure that the value is not in the right subarray)
5. If the middle element is smaller than the desired value AND
   the max is on the right side, then do bitonic search on the right subarray
6. If the middle element is bigger than the desired value, then do descending
   binary search on the right subarray and ascending binary search on the left subarray.
*/
// SOLUTION_END

public class BitonicSearchArray {
    public static int bitonicSearch(int key, int[] a) {
        // SOLUTION_BEGIN
        return bitonicSearch(key, a, 0, a.length-1);
        // SOLUTION_END
    }

    // SOLUTION_BEGIN
    private static int bitonicSearch(int key, int[] a, int lo, int hi) {
        if (lo > hi) return -1;
        int mid = lo + (hi - lo) / 2;
        if (a[mid] < key) {
            // where is max
            if (a[mid-1] < a[mid]) {
                // on the right
                return bitonicSearch(key, a, mid+1, hi);
            } else {
                // on the left
                return bitonicSearch(key, a, lo, mid-1);
            }
        } else if (key < a[mid]) {
            int pos = binarySearchDESC(key, a, mid+1, hi);
            if (pos == -1) {
                return binarySearchASC(key, a, lo, mid-1);
            } else {
                return pos;
            }
        } else {
            return mid;
        }
    }

    public static int binarySearchASC(int key, int[] a, int lo, int hi) {
        if (lo > hi) return -1;
        int mid = lo + (hi - lo) / 2;
        if (key < a[mid]) {
            return binarySearchASC(key, a, lo, mid-1);
        } else if (a[mid] < key) {
            return binarySearchASC(key, a, mid+1, hi);
        } else {
            return mid;
        }
    }

    public static int binarySearchDESC(int key, int[] a, int lo, int hi) {
        if (lo > hi) return -1;
        int mid = lo + (hi - lo) / 2;
        if (key < a[mid]) {
            return binarySearchDESC(key, a, mid+1, hi);
        } else if (a[mid] < key) {
            return binarySearchDESC(key, a, lo, mid-1);
        } else {
            return mid;
        }
    }
    // SOLUTION_END

    public static void main(String[] args) throws Throwable {
        Assert.assertEquals(-1, bitonicSearch(0, new int[] {1,3,5,7,6,4,2}));
        Assert.assertEquals(-1, bitonicSearch(8, new int[] {1,3,5,7,6,4,2}));
        Assert.assertEquals(0, bitonicSearch(1, new int[] {1,3,5,7,6,4,2}));
        Assert.assertEquals(2, bitonicSearch(5, new int[] {1,3,5,7,6,4,2}));
        Assert.assertEquals(5, bitonicSearch(4, new int[] {1,3,5,7,6,4,2}));
        Assert.assertEquals(6, bitonicSearch(2, new int[] {1,3,5,7,6,4,2}));
        System.out.println("OK");
    }
}