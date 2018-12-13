class MergeSortBottomUpArray {
    @SuppressWarnings("unchecked")
    public static <T extends Comparable<T>> T[] sort(T[] a) {
        int n = a.length;
        T[] aux = (T[]) new Comparable[n];
        // SOLUTION_BEGIN
        for (int len = 1; len < n; len *= 2)
            for (int lo = 0; lo < n-len; lo += 2*len)
                merge(a, aux, lo, lo+len-1, Math.min(lo+len+len-1, n-1));
        return a;
        // SOLUTION_END
    }

    // SOLUTION_BEGIN
    private static <T extends Comparable<T>> void merge(T[] a, T[] aux, int lo, int mid, int hi) {
        int i = lo, j = mid + 1;
        for (int k = lo; k <= hi; k++)
            aux[k] = a[k];

        for (int k = lo; k <= hi; k++) {
            if      (i > mid)              a[k] = aux[j++];
            else if (j > hi)               a[k] = aux[i++];
            else if (less(aux[j], aux[i])) a[k] = aux[j++];
            else                           a[k] = aux[i++];
        }
    }
    // SOLUTION_END

    private static <T extends Comparable<T>> boolean less(T v, T w) {
        return v.compareTo(w) < 0;
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertArrayEquals(new Integer[]{},
                                 sort(new Integer[] {}));
        Assert.assertArrayEquals(new Integer[]{1},
                                 sort(new Integer[] {1}));
        Assert.assertArrayEquals(new Integer[]{1,2,3,4,5,6},
                                 sort(new Integer[] {6,5,4,3,2,1}));
        Assert.assertArrayEquals(new Integer[]{1,2,3,4,5,6},
                                 sort(new Integer[] {1,2,3,4,5,6}));
        Assert.assertArrayEquals(new Integer[]{1,2,3,4,5,6},
                                 sort(ShuffleArray.shuffle(new Integer[] {1,2,3,4,5,6})));
        System.out.println("OK");
    }
}
