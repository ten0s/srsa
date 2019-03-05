public class StringSearchBruteForce {
    public static int search(String pat, String txt) {
        //+BEGIN_SOLUTION
        int m = pat.length();
        int n = txt.length();
        for (int i = 0; i <= n-m; i++) {
            int j;
            for (j = 0; j < m; j++) {
                if (txt.charAt(i+j) != pat.charAt(j))
                    break;
            }
            if (j == m) return i;
        }
        return -1;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Assert.assertEquals("abc".indexOf("")            , search("", "abc"));
        Assert.assertEquals("".indexOf("abc")            , search("abc", ""));
        Assert.assertEquals("abc".indexOf("abc")         , search("abc", "abc"));
        Assert.assertEquals("xabcx".indexOf("abc")       , search("abc", "xabcx"));
        Assert.assertEquals("abacadabrac".indexOf("abra"), search("abra", "abacadabrac"));
        System.out.println("OK");
    }
    //+END_FOLD }
}
