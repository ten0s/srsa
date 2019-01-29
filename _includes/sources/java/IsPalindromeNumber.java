class IsPalindromeNumber {
    public static boolean isPalindrome(int n) {
        // SOLUTION_BEGIN
        if (n < 0) return false;
        if (n < 10) return true;
        int base = (int) Math.pow(10, (int) Math.log10(n));
        int big = n / base;
        int little = n % 10;
        if (big == little) {
            int m = (n - big * base - little) / 10;
            return isPalindrome(m);
        }
        return false;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertFalse(isPalindrome(-3));
        Assert.assertTrue(isPalindrome(3));
        Assert.assertTrue(isPalindrome(232));
        Assert.assertTrue(isPalindrome(24542));
        Assert.assertFalse(isPalindrome(123));
        Assert.assertFalse(isPalindrome(1000021));
        System.out.println("OK");
    }
}
