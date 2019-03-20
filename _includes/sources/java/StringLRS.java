/*
public class SuffixArray {
    public SuffixArray(String text);
    public int index(int i );
    public int length();
    public int lcp(int i);
}
*/

public class StringLRS {
    public static String lrs(String s) {
        //+BEGIN_SOLUTION
        SuffixArray sa = new SuffixArray(s);
        int n = s.length();
        String lrs = "";
        for (int i = 1; i < n; i++) {
            int length = sa.lcp(i);
            if (length > lrs.length())
                lrs = s.substring(sa.index(i), sa.index(i)+length);
        }
        return lrs;
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
