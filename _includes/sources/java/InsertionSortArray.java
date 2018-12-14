class InsertionSortArray {
    public static <T extends Comparable<T>> T[] sort(T[] a) {
        // SOLUTION_BEGIN
        int n = a.length;
        for (int i = 1; i < n; i++) {
            for (int j = i; j > 0 && less(a[j], a[j-1]); j--) {
                swap(a, j, j-1);
            }
        }
        return a;
        // SOLUTION_END
    }

    private static <T extends Comparable<T>> boolean less(T v, T w) {
        return v.compareTo(w) < 0;
    }

    private static <T extends Comparable<T>> void swap(T[] a, int i, int j) {
        T tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
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
