public class Main {
    public static int gcd(int a, int b) {

    }

    public static void main(String[] args) {
        try {
            assert 14 == gcd(42, 56);
            assert 6 == gcd(18, 84);
            assert 2 == gcd(-4, 14);
            System.out.println("OK");
        } catch (Throwable e) {
            System.out.println("FAIL");
            e.printStackTrace();
        }
    }
}
