//+BEGIN_SOLUTION
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
//+END_SOLUTION

public class ArrayBitonicSearch {
    public static int bitonicSearch(int key, int[] a) {
        return bitonicSearch(key, a, 0, a.length-1);
    }

    private static int bitonicSearch(int key, int[] a, int lo, int hi) {
        //+BEGIN_SOLUTION
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
        //+END_SOLUTION
    }

    public static int binarySearchASC(int key, int[] a, int lo, int hi) { //+BEGIN_FOLD
        if (lo > hi) return -1;
        int mid = lo + (hi - lo) / 2;
        if (key < a[mid]) {
            return binarySearchASC(key, a, lo, mid-1);
        } else if (a[mid] < key) {
            return binarySearchASC(key, a, mid+1, hi);
        } else {
            return mid;
        }
    } //+END_FOLD To Use

    public static int binarySearchDESC(int key, int[] a, int lo, int hi) { //+BEGIN_FOLD
        if (lo > hi) return -1;
        int mid = lo + (hi - lo) / 2;
        if (key < a[mid]) {
            return binarySearchDESC(key, a, mid+1, hi);
        } else if (a[mid] < key) {
            return binarySearchDESC(key, a, lo, mid-1);
        } else {
            return mid;
        }
    } //+END_FOLD To Use


    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        int[] a = new int[] {8,9,10,7,6,5,4,3,2,1};
        Assert.assertEquals(-1, bitonicSearch(0, a));
        Assert.assertEquals(-1, bitonicSearch(11, a));
        Assert.assertEquals(0, bitonicSearch(8, a));
        Assert.assertEquals(9, bitonicSearch(1, a));

        int[] b = new int[] {1,2,3,4,5,6,7,10,9,8};
        Assert.assertEquals(-1, bitonicSearch(0, b));
        Assert.assertEquals(-1, bitonicSearch(11, b));
        Assert.assertEquals(9, bitonicSearch(8, b));
        Assert.assertEquals(0, bitonicSearch(1, b));
        System.out.println("OK");
    }
    //+END_FOLD }
}
