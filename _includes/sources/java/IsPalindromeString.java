class IsPalindromeString {
    public static boolean isPalindrome(String s) {
        //+BEGIN_SOLUTION
        int n = s.length();
        for (int i = 0; i < n/2; i++) {
            if (s.charAt(i) != s.charAt(n-i-1))
                return false;
        }
        return true;
        //+END_SOLUTION
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertTrue(isPalindrome(""));
        Assert.assertTrue(isPalindrome("a"));
        Assert.assertTrue(isPalindrome("abcba"));
        Assert.assertFalse(isPalindrome("abcde"));
        System.out.println("OK");
    }
}
