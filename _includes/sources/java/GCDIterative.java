public class GCDIterative {
    public static int gcd(int a, int b) {
        //+BEGIN_SOLUTION
        while (b != 0) {
            int r = a % b;
            a = b;
            b = r;
        }
        return Math.abs(a);
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Assert.assertEquals(14, gcd(42, 56));
        Assert.assertEquals(6, gcd(18, 84));
        Assert.assertEquals(2, gcd(-4, 14));
        System.out.println("OK");
    }
    //+END_FOLD }
}
