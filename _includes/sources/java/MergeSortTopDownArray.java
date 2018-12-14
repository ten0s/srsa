class MergeSortTopDownArray {
    @SuppressWarnings("unchecked")
    public static <T extends Comparable<T>> T[] sort(T[] a) {
        T[] aux = (T[]) new Comparable[a.length];
        return sort(a, aux, 0, a.length-1);
    }

    private static <T extends Comparable<T>> T[] sort(T[] a, T[] aux, int lo, int hi) {
        // SOLUTION_BEGIN
        if (lo >= hi) return a;
        int mid = lo + (hi - lo) / 2;
        sort(a, aux, lo, mid);
        sort(a, aux, mid+1, hi);
        merge(a, aux, lo, mid, hi);
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
        Assert.assertArrayEquals(new Integer[] {},
                                 sort(new Integer[] {}));
        Assert.assertArrayEquals(new Integer[] {1},
                                 sort(new Integer[] {1}));
        Assert.assertArrayEquals(new Integer[] {1,2,3,4,5,6},
                                 sort(new Integer[] {6,5,4,3,2,1}));
        Assert.assertArrayEquals(new Integer[] {1,2,3,4,5,6},
                                 sort(new Integer[] {1,2,3,4,5,6}));
        Assert.assertArrayEquals(new Integer[] {1,2,3,4,5,6},
                                 sort(ArrayUtil.shuffle(new Integer[] {1,2,3,4,5,6})));
        System.out.println("OK");
    }
}
