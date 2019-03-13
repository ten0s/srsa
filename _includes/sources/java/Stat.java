public class Stat {
    public static double mean(int[] a) {
        //+BEGIN_SOLUTION
        int n = a.length;
        double sum = 0.0;
        for (int i = 0; i < n; i++)
            sum += a[i];
        return sum / n;
        //+END_SOLUTION
    }

    public static double variance(int[] a) {
        //+BEGIN_SOLUTION
        int n = a.length;
        double mean = mean(a);
        double sum = 0.0;
        for (int i = 0; i < n; i++)
            sum += Math.pow(a[i] - mean, 2);
        return sum / n;
        //+END_SOLUTION
    }

    public static double stdDev(int[] a) {
        //+BEGIN_SOLUTION
        return Math.sqrt(variance(a));
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        int[] a = new int[] {74, 66, 68, 69, 73, 70};
        Assert.assertEquals(70.0, mean(a)    , 0.1);
        Assert.assertEquals(7.7 , variance(a), 0.1);
        Assert.assertEquals(2.8 , stdDev(a)  , 0.1);

        int[] b = new int[] {65, 68, 69, 70, 71, 75};
        Assert.assertEquals(69.6, mean(b)    , 0.1);
        Assert.assertEquals(9.3 , variance(b), 0.1);
        Assert.assertEquals(3.0 , stdDev(b)  , 0.1);

        System.out.println("OK");
    }
    //+END_FOLD }
}
