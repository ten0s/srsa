public class StringReverse {
    public static String reverse(String s) {
        //+BEGIN_SOLUTION
        StringBuilder sb = new StringBuilder();
        for (int i = s.length()-1; i >= 0; i--) {
            sb.append(s.charAt(i));
        }
        return sb.toString();
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Assert.assertEquals("", reverse(""));
        Assert.assertEquals("1", reverse("1"));
        Assert.assertEquals("54321", reverse("12345"));
        System.out.println("OK");
    }
    //+END_FOLD }
}
