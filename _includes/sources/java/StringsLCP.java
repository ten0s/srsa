public class StringsLCP {
    public static int lcp(String s1, String s2) {
        //+BEGIN_SOLUTION
        int n = Math.min(s1.length(), s2.length());
        for (int i = 0; i < n; i++) {
            if (s1.charAt(i) != s2.charAt(i)) {
                return i;
            }
        }
        return n;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Assert.assertEquals(0, lcp("", ""));
        Assert.assertEquals(0, lcp("", "prefix"));
        Assert.assertEquals(0, lcp("prefetch", ""));
        Assert.assertEquals(4, lcp("prefetch", "prefix"));
        System.out.println("OK");
    }
    //+END_FOLD }
}
