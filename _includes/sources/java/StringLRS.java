public class StringLRS {
    public static String lrs(String s) {
        //+BEGIN_SOLUTION
        SuffixArray sa = new SuffixArray(s);
        int n = s.length();
        int length = 0;
        int index = 0;
        for (int i = 1; i < n; i++) {
            int len = sa.lcp(i);
            if (len > length) {
                length = len;
                index = sa.index(i);
            }
        }
        return s.substring(index, index + length);
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Assert.assertEquals(""    , lrs("abc"));
        Assert.assertEquals("abc" , lrs("abcabc"));
        Assert.assertEquals("abcd", lrs("aababcabcdabcde"));
        Assert.assertEquals("issi", lrs("mississippi"));
        System.out.println("OK");
    }
    //+END_FOLD }
}

// Refs
/*+BEGIN_FOLD
public class SuffixArray {
    public SuffixArray(String text);
    public int index(int i );
    public int length();
    public int lcp(int i);
}
+END_FOLD*/
