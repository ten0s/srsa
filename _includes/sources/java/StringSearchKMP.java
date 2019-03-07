public class StringSearchKMP {
    private static class KMP {
        //+BEGIN_SOLUTION
        private String pat;
        private int[][] dfa;
        //+END_SOLUTION

        public KMP(String pat) {
            //+BEGIN_SOLUTION
            this.pat = pat;
            int R = 256;
            int M = pat.length();
            dfa = new int[R][M];
            dfa[pat.charAt(0)][0] = 1;
            for (int X = 0, j = 1; j < M; j++) {
                for (char c = 0; c < R; c++)
                    dfa[c][j] = dfa[c][X];       // Copy mismatch cases
                dfa[pat.charAt(j)][j] = j+1;     // Set match case
                X = dfa[pat.charAt(j)][X];       // Update restart state
            }
            //+END_SOLUTION
        }

        public int search(String txt) {
            //+BEGIN_SOLUTION
            int M = pat.length();
            int N = txt.length();
            int i, j;
            for (i = 0, j = 0; i < N && j < M; i++)
                j = dfa[txt.charAt(i)][j];
            if (j == M) return i-M;
            return -1;
            //+END_SOLUTION
        }
    }

    public static int search(String pat, String txt) {
        //+BEGIN_SOLUTION
        if (pat.length() == 0) return 0;
        KMP kmp = new KMP(pat);
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
