public class StringSearchBruteForce {
    public static int search(String pat, String txt) {
        //+BEGIN_SOLUTION
        int M = pat.length();
        int N = txt.length();
        for (int i = 0; i <= N-M; i++) {
            int j;
            for (j = 0; j < M; j++) {
                if (txt.charAt(i+j) != pat.charAt(j))
                    break;
            }
            if (j == M) return i;
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
