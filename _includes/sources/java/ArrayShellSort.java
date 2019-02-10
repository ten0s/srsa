class ArrayShellSort {
    public static <T extends Comparable<T>> T[] sort(T[] a) {
        //+BEGIN_SOLUTION
        int n = a.length;
        int h = 1;
        while (h < n/3) h = 3*h + 1; // 1, 4, 13, 40, 121, ...
        while (h >= 1) {
            for (int i = h; i < n; i++) {
                for (int j = i; j-h >= 0 && less(a[j], a[j-h]); j = j-h) {
                    swap(a, j, j-h);
                }
            }
            h /= 3;
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

    //+BEGIN_FOLD Tests {
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
    //+END_FOLD }
}
