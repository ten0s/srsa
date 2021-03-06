public class ArrayShuffle {
    public static int[] shuffle(int[] a) {
        //+BEGIN_SOLUTION
        // Knuth's shuffle O(n)
        int n = a.length;
        for (int i = 1; i < n; i++) {
            // choose uniformly [0, i]
            int r = (int) (Math.random() * (i + 1));
            int tmp = a[i];
            a[i] = a[r];
            a[r] = tmp;
        }
        return a;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        final int N = 1000;
        final int[] asc  = new int[] {1,2,3,4,5,6,7,8,9,10};
        final int[] desc = new int[] {10,9,8,7,6,5,4,3,2,1};
        int[] sumsAsc  = new int[asc.length];
        int[] sumsDesc = new int[asc.length];
        for (int i = 0; i < N; i++) {
            final int[] shuffledAsc = shuffle(ArrayUtil.copy(asc));
            final int[] shuffledDesc = shuffle(ArrayUtil.copy(desc));
            for (int j = 0; j < asc.length; j++) {
                sumsAsc[j]  += shuffledAsc[j];
                sumsDesc[j] += shuffledDesc[j];
            }
        }
        System.out.println(ArrayUtil.toString(sumsAsc));
        Histogram.print(sumsAsc);
        System.out.println(ArrayUtil.toString(sumsDesc));
        Histogram.print(sumsDesc);

        double stdDevAsc = Stat.stdDev(sumsAsc);
        Assert.assertTrue("" + stdDevAsc, stdDevAsc < 150.0);
        double stdDevDesc = Stat.stdDev(sumsDesc);
        Assert.assertTrue("" + stdDevDesc, stdDevDesc < 150.0);

        System.out.println("OK");
    }
    //+END_FOLD }
}
