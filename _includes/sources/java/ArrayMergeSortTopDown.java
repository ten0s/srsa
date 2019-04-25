class ArrayMergeSortTopDown {
    @SuppressWarnings("unchecked")
    public static <T extends Comparable<T>> T[] sort(T[] a) {
        T[] aux = (T[]) new Comparable[a.length];
        return sort(a, aux, 0, a.length-1);
    }

    private static <T extends Comparable<T>> T[] sort(T[] a, T[] aux, int lo, int hi) {
        //+BEGIN_SOLUTION
        if (lo >= hi) return a;
        int mid = lo + (hi - lo) / 2;
        sort(a, aux, lo, mid);
        sort(a, aux, mid+1, hi);
        merge(a, aux, lo, mid, hi);
        return a;
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private static <T extends Comparable<T>> T[] merge(T[] a, T[] aux, int lo, int mid, int hi) {
        for (int k = lo; k <= hi; k++) {
            aux[k] = a[k];
        }
        int i = lo, j = mid + 1;
        for (int k = lo; k <= hi; k++) {
            if      (i > mid)              a[k] = aux[j++];
            else if (j > hi)               a[k] = aux[i++];
            else if (less(aux[j], aux[i])) a[k] = aux[j++];
            else                           a[k] = aux[i++];
        }
        return a;
    }
    //+END_SOLUTION

    private static <T extends Comparable<T>> boolean less(T v, T w) {
        return v.compareTo(w) < 0;
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // merge tests
        Assert.assertArrayEquals(new Integer[] {1},
                                 merge(new Integer[] {1}, new Integer[1], 0, 0, 0));
        Assert.assertArrayEquals(new Integer[] {1,2},
                                 merge(new Integer[] {2,1}, new Integer[2], 0, 0, 1));
        Assert.assertArrayEquals(new Integer[] {1,2,4,3},
                                 merge(new Integer[] {2,1,4,3}, new Integer[4], 0, 0, 1));
        Assert.assertArrayEquals(new Integer[] {2,1,3,4},
                                 merge(new Integer[] {2,1,4,3}, new Integer[4], 2, 2, 3));
        Assert.assertArrayEquals(new Integer[] {1,2,3,4},
                                 merge(new Integer[] {3,4,1,2}, new Integer[4], 0, 1, 3));
        // sort tests
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
    //+END_FOLD }
}
