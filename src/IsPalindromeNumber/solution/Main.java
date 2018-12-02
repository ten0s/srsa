class Main {
    public static boolean isPalindrome(int n) {
        if (n < 0) return isPalindrome(-n);
        if (n < 10) return true;
        int base = (int) Math.pow(10, (int) Math.log10(n));
        int big = n / base;
        int little = n % 10;
        if (big == little) {
            int m = (n - big * base - little) / 10;
            return isPalindrome(m);
        }
        return false;
    }

    public static void main(String[] args) {
        try {
            assert isPalindrome(-3);
            assert isPalindrome(3);
            assert isPalindrome(121);
            assert isPalindrome(232);
            assert isPalindrome(24542);
            assert !isPalindrome(123);
            System.out.println("OK");
        } catch (Throwable e) {
            System.out.println("FAIL");
            e.printStackTrace();
        }

    }
}
