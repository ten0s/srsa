public class Main {
    public static boolean isRotation(String s1, String s2) {

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
