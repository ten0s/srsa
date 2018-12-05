public class RotateString {
    public static String rotate(int n, String s) {
        // SOLUTION_BEGIN
        int len = s.length();
        if (len == 0) return s;
        int m = n % len;
        if (m == 0) return s;
        String a = s.substring(0, m);
        String b = s.substring(m, len);
        return b + a;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertEquals("", rotate(1, ""));
        Assert.assertEquals("1", rotate(1, "1"));
        Assert.assertEquals("23451", rotate(1, "12345"));
        Assert.assertEquals("45123", rotate(3, "12345"));
        Assert.assertEquals("12345", rotate(5, "12345"));
        Assert.assertEquals("12345", rotate(10, "12345"));
        System.out.println("OK");
    }
}
