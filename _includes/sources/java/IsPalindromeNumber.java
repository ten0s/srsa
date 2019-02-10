class IsPalindromeNumber {
    public static boolean isPalindrome(int n) {
        //+BEGIN_SOLUTION
        if (n < 0) return false;
        if (n < 10) return true;
        int p = (int) Math.log10(n);
        for (int i = 0; i <= p/2; i++) {
            if (digit(i, n) != digit(p-i, n)) return false;
        }
        return true;
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private static int digit(int i, int n) {
        return (int) (n / Math.pow(10, i)) % 10;
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Assert.assertFalse(isPalindrome(-3));
        Assert.assertTrue(isPalindrome(3));
        Assert.assertFalse(isPalindrome(10));
        Assert.assertTrue(isPalindrome(232));
        Assert.assertTrue(isPalindrome(24542));
        Assert.assertFalse(isPalindrome(123));
        Assert.assertFalse(isPalindrome(1000021));
        System.out.println("OK");
    }
    //+END_FOLD }
}
