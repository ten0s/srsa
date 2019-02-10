//+BEGIN_SOLUTION
/*
http://flexaired.blogspot.com/2013/02/local-minimum-of-array.html
https://www.geeksforgeeks.org/find-local-minima-array/

Solution:
If a[0] < a[1] or a[N-2] > a[N-1] then a[0] or a[N-1] is LM, respectively.

Otherwise, pick a[mid] where mid = N/2. If it's not the answer, we have three cases:
1)  If a[mid-1] < a[mid] < a[mid+1], lower half will contain an LM.
2)  If a[mid-1] > a[mid] > a[mid+1], upper half will contain an LM.
3)  If a[mid-1] < a[mid] > a[mid+1], either half will contain an LM.

Search on the new interval recursively.

Explanation:
If a[1] < a[2], a[1] is an LM. Otherwise, if a[n-1] > a[n], a[n] is an LM.
Otherwise, the array entries start out descending (going from a[1] to a[2]) and
ends up ascending (going from a[n-1] to a[n]). It follows that an LM must exist
somewhere in between.  Examine a[n/2].  If a[n/2] > a[n/2+1], then by the same
reasoning there is an LM in the upper half of the array.  Similarly,
if a[n/2] < a[n/2+1] there is an LM in the lower half of the array.
Solve recursively in the appropriate half.
*/
//+END_SOLUTION

public class ArrayLocalMinimum {
    public static int indexOf(int[] a) {
        //+BEGIN_SOLUTION
        return indexOf(a, 0, a.length-1);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
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
                return lo;
            } else if (diff > 0) {
                return hi;
            } else {
                // Some not distinct values
                throw new IllegalArgumentException();
            }
        }

        int mid = lo + (hi - lo) / 2;
        if (a[mid-1] > a[mid] && a[mid] < a[mid+1]) {
            return mid;
        } else if (a[mid-1] < a[mid]) {
            return indexOf(a, lo, mid-1);
        } else {
            return indexOf(a, mid+1, hi);
        }
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Assert.assertEquals(-1, indexOf(new int[] {}));
        Assert.assertEquals(0, indexOf(new int[] {1}));
        Assert.assertEquals(0, indexOf(new int[] {1,2}));
        Assert.assertEquals(1, indexOf(new int[] {2,1}));
        Assert.assertEquals(1, indexOf(new int[] {2,1,3}));
        Assert.assertEquals(0, indexOf(new int[] {1,2,3,4,5}));
        Assert.assertEquals(4, indexOf(new int[] {5,4,3,2,1}));
        Assert.assertEquals(0, indexOf(new int[] {4,5,6,9,8,10,12}));
        Assert.assertEquals(2, indexOf(new int[] {9,7,2,8,5,6,3,4}));
        System.out.println("OK");
    }
    //+END_FOLD }
}
