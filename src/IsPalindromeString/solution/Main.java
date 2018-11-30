class Main {
    public static boolean isPalindrome(String s) {
        int n = s.length();
        for (int i = 0; i < n/2; i++) {
            if (s.charAt(i) != s.charAt(n-i-1))
                return false;
        }
        return true;
    }

    public static void main(String[] args) {
        try {
            assert isPalindrome("");
            assert isPalindrome("a");
            assert isPalindrome("abcba");
            assert !isPalindrome("abcde");
            System.out.println("OK");
        } catch (Throwable e) {
            System.out.println("FAIL");
            e.printStackTrace();
        }
    }
}
