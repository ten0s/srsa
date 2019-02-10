class ArraySelectionSort {
    public static <T extends Comparable<T>> T[] sort(T[] a) {
        //+BEGIN_SOLUTION
        int n = a.length;
        for (int i = 0; i < n; i++) {
            int min = i;
            for (int j = i+1; j < n; j++) {
                if (less(a[j], a[min]))
                    min = j;
            }
            swap(a, i, min);
        }
        return a;
        //+END_SOLUTION
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
        Assert.assertArrayEquals(new Integer[]{},
                                 sort(new Integer[] {}));
        Assert.assertArrayEquals(new Integer[]{1},
                                 sort(new Integer[] {1}));
        Assert.assertArrayEquals(new Integer[]{1,2,3,4,5,6},
                                 sort(new Integer[] {6,5,4,3,2,1}));
        Assert.assertArrayEquals(new Integer[]{1,2,3,4,5,6},
                                 sort(new Integer[] {1,2,3,4,5,6}));
        Assert.assertArrayEquals(new Integer[]{1,2,3,4,5,6},
                                 sort(ArrayUtil.shuffle(new Integer[] {1,2,3,4,5,6})));
        System.out.println("OK");
    }
}
