public class GCD {
    public static int gcd(int a, int b) {
        // SOLUTION_BEGIN
        if (b == 0) {
            return Math.abs(a);
        } else {
            return gcd(b, a % b);
        }
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertEquals(14, gcd(42, 56));
        Assert.assertEquals(6, gcd(18, 84));
        Assert.assertEquals(2, gcd(-4, 14));
        System.out.println("OK");
    }
}
