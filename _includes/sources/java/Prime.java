public class Prime {
    public static boolean isPrime(long n) {
        // SOLUTION_BEGIN
        if (n < 2) return false;
        for (long f = 2; f*f <= n; f++) {
            if (n % f == 0) {
                return false;
            }
        }
        return true;
        // SOLUTION_END
    }

    public static long nextPrime(long n) {
        // SOLUTION_BEGIN
        if (n < 2) return 2;
        long p = n + 1;
        if (p % 2 == 0) {
            p += 1;
        }
        while (!isPrime(p)) {
            p += 2;
        }
        return p;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertFalse(isPrime(1));
        Assert.assertTrue(isPrime(2));
        Assert.assertTrue(isPrime(3));
        Assert.assertFalse(isPrime(4));
        Assert.assertTrue(isPrime(5));
        Assert.assertTrue(isPrime(17));
        Assert.assertFalse(isPrime(81));

        Assert.assertEquals(2, nextPrime(1));
        Assert.assertEquals(3, nextPrime(2));
        Assert.assertEquals(5, nextPrime(3));
        Assert.assertEquals(7, nextPrime(5));
        Assert.assertEquals(11, nextPrime(7));

        System.out.println("OK");
    }
}
