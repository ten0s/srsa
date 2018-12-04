class Main {
    public static boolean isPalindrome(int n) {

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
