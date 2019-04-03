import java.util.*;

public class StringKWIC {
    public static Iterable<String> kwic(String text, int context, String query) {
        //+BEGIN_SOLUTION
        Queue<String> q = new ArrayDeque<>();
        SuffixArray sa = new SuffixArray(text);
        int n = sa.length();
        for (int i = sa.rank(query); i < n; i++) {
            // check if sorted suffix i is a match
            int from1 = sa.index(i);
            int to1   = Math.min(n, sa.index(i) + query.length());
            if (!query.equals(text.substring(from1, to1))) break;

            // add context surrounding sorted suffix i
            int from2 = Math.max(0, sa.index(i) - context);
            int to2   = Math.min(n, sa.index(i) + context + query.length());
            q.add(text.substring(from2, to2));
        }
        return q;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        String text = "The quick brown fox jumps over the lazy dog";
        Assert.assertArrayEquals(new String[] {"ver the lazy", "The quic"},
                                 ArrayUtil.toArray(kwic(text, 5, "he")));
        Assert.assertArrayEquals(new String[] {"over the lazy"},
                                 ArrayUtil.toArray(kwic(text, 5, "the")));
        Assert.assertArrayEquals(new String[] {"lazy dog"},
                                 ArrayUtil.toArray(kwic(text, 5, "dog")));
        System.out.println("OK");
    }
    //+END_FOLD }
}

// Refs
/*+BEGIN_FOLD
public class SuffixArray {
    public SuffixArray(String text);
    public int index(int i );
    public int rank(String query);
}
+END_FOLD*/
