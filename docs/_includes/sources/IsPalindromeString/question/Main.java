class Main {
    public static boolean isPalindrome(String s) {

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
