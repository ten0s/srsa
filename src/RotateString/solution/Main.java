public class Main {
    public static String rotate(int n, String s) {
        int len = s.length();
        if (len == 0) return s;
        int m = n % len;
        if (m == 0) return s;
        String a = s.substring(0, m);
        String b = s.substring(m, len);
        return b + a;
    }

    public static void main(String[] args) {
        try {
            assert "".equals(rotate(1, ""));
            assert "1".equals(rotate(1, "1"));
            assert "23451".equals(rotate(1, "12345"));
            assert "45123".equals(rotate(3, "12345"));
            assert "12345".equals(rotate(5, "12345"));
            assert "12345".equals(rotate(10, "12345"));
            System.out.println("OK");
        } catch (Throwable e) {
            System.out.println("FAIL");
            e.printStackTrace();
        }
    }
}
