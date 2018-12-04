public class Main {
    public static boolean isRotation(String s1, String s2) {
        if (s1.length() != s2.length())
            return false;
        return (s1 + s1).indexOf(s2) != -1;
    }

    public static void main(String[] args) {
        try {
            assert isRotation("ABCDE", "CDEAB");
            assert !isRotation("ABCDE", "DCEAB");
            System.out.println("OK");
        } catch (Throwable e) {
            System.out.println("FAIL");
            e.printStackTrace();
        }
    }
}
