public class IsRotationString {
    public static boolean isRotation(String s1, String s2) {
        // SOLUTION_BEGIN
        if (s1.length() != s2.length())
            return false;
        return (s1 + s1).indexOf(s2) != -1;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        Assert.assertTrue(isRotation("ABCDE", "CDEAB"));
        Assert.assertFalse(isRotation("ABCDE", "DCEAB"));
        System.out.println("OK");
    }
}
