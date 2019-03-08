public class StringSearchBoyerMoore {
    private static class BoyerMoore {
        //+BEGIN_SOLUTION
        private String pat;
        private int[] right;
        //+END_SOLUTION

        public BoyerMoore(String pat) {
            //+BEGIN_SOLUTION
            this.pat = pat;
            int R = 256;
            int M = pat.length();
            right = new int[R];
            for (char c = 0; c < R; c++)
                right[c] = -1;
            for (int j = 0; j < M; j++)
                right[pat.charAt(j)] = j;
            //+END_SOLUTION
        }

        public int search(String txt) {
            //+BEGIN_SOLUTION
            int M = pat.length();
            int N = txt.length();
            int skip;
            for (int i = 0; i <= N-M; i += skip) {
                skip = 0;
                for (int j = M-1; j >= 0; j--) {
                    if (pat.charAt(j) != txt.charAt(i+j)) {
                        skip = Math.max(1, j-right[txt.charAt(i+j)]);
                        break;
                    }
                }
                if (skip == 0) return i;
            }
            return -1;
            //+END_SOLUTION
        }
    }

    public static int search(String pat, String txt) {
        //+BEGIN_SOLUTION
        BoyerMoore kmp = new BoyerMoore(pat);
        return kmp.search(txt);
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
