public class IsRotationString {
    public static boolean isRotation(String s1, String s2) {
        //+BEGIN_SOLUTION
        if (s1.length() != s2.length())
            return false;
        return (s1 + s1).indexOf(s2) != -1;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Assert.assertTrue(isRotation("ABCDE", "CDEAB"));
        Assert.assertFalse(isRotation("ABCDE", "DCEAB"));
        System.out.println("OK");
    }
    //+END_FOLD }
}
