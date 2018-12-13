class QuickSortArray {
    public static <T extends Comparable<T>> T[] sort(T[] a) {
        ShuffleArray.shuffle(a);
        return sort(a, 0, a.length-1);
    }

    private static <T extends Comparable<T>> T[] sort(T[] a, int lo, int hi) {
        // SOLUTION_BEGIN
        if (lo >= hi) return a;
        int j = partition(a, lo, hi);
        sort(a, lo, j-1);
        sort(a, j+1, hi);
        return a;
        // SOLUTION_END
    }

    // SOLUTION_BEGIN
    private static <T extends Comparable<T>> int partition(T[] a, int lo, int hi) {
        int i = lo, j = hi+1;
        T v = a[lo];
        while (true) {
            while (less(a[++i], v))
                if (i == hi) break;

            while (less(v, a[--j]))
                if (j == lo) break;

            if (i >= j)
                break;
            swap(a, i, j);
        }
        swap(a, lo, j);
        return j;
    }
    // SOLUTION_END

    private static <T extends Comparable<T>> boolean less(T v, T w) {
        return v.compareTo(w) < 0;
    }

    private static <T extends Comparable<T>> void swap(T[] a, int i, int j) {
        T tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
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
