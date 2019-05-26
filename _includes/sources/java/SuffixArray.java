import java.util.*;

public class SuffixArray {
    private class Suffix implements Comparable<Suffix> {
        private final String text;
        private final int index;

        private Suffix(String text, int index) {
            this.text = text;
            this.index = index;
        }

        private int length() {
            //+BEGIN_SOLUTION
            return text.length() - index;
            //+END_SOLUTION
        }

        private char charAt(int i) {
            //+BEGIN_SOLUTION
            return text.charAt(index + i);
            //+END_SOLUTION
        }

        public String toString() {
            //+BEGIN_SOLUTION
            return text.substring(index);
            //+END_SOLUTION
        }

        public int compareTo(Suffix that) {
            //+BEGIN_SOLUTION
            if (this == that) return 0;
            int n = Math.min(this.length(), that.length());
            for (int i = 0; i < n; i++) {
                if (this.charAt(i) < that.charAt(i)) return -1;
                if (this.charAt(i) > that.charAt(i)) return +1;
            }
            return this.length() - that.length();
            //+END_SOLUTION
        }
    }

    private Suffix[] suffixes;
    private String text;

    public SuffixArray(String text) {
        this.text = text;
        //+BEGIN_SOLUTION
        int n = text.length();
        suffixes = new Suffix[n];
        for (int i = 0; i < n; i++)
            suffixes[i] = new Suffix(text, i);
        Arrays.sort(suffixes);
        //+END_SOLUTION
    }

    public int index(int i ) {
        //+BEGIN_SOLUTION
        return suffixes[i].index;
        //+END_SOLUTION
    }

    public int length() {
        //+BEGIN_SOLUTION
        return suffixes.length;
        //+END_SOLUTION
    }

    public String select(int i) {
        //+BEGIN_SOLUTION
        return suffixes[i].toString();
        //+END_SOLUTION
    }

    public int lcp(int i) {
        //+BEGIN_SOLUTION
        Suffix s1 = suffixes[i-1];
        Suffix s2 = suffixes[i];
        int n = Math.min(s1.length(), s2.length());
        for (int j = 0; j < n; j++)
            if (s1.charAt(j) != s2.charAt(j)) return j;
        return n;
        //+END_SOLUTION
    }

    public int rank(String key) {
        //+BEGIN_SOLUTION
        int lo = 0, hi = suffixes.length-1;
        while (lo <= hi) {
            int mid = lo + (hi - lo) / 2;
            int cmp = compare(key, suffixes[mid]);
            if      (cmp < 0) hi = mid-1;
            else if (cmp > 0) lo = mid+1;
            else              return mid;
        }
        return lo;
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private int compare(String s1, Suffix s2) {
        int n = Math.min(s1.length(), s2.length());
        for (int i = 0; i < n; i++) {
            if (s1.charAt(i) < s2.charAt(i)) return -1;
            if (s1.charAt(i) > s2.charAt(i)) return +1;
        }
        return s1.length() - s2.length();
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Utils {
    public void debug() {
        System.out.println("  i ind lcp rnk select");
        System.out.println("---------------------------");
        for (int i = 0; i < text.length(); i++) {
            int index = index(i);
            String ith =
                Character.toString('"') +
                text.substring(index, Math.min(index + 50, text.length())) +
                Character.toString('"');
            int rank = rank(text.substring(index));
            if (i == 0) {
                System.out.printf("%3d %3d %3s %3d %s", i, index, "-", rank, ith);
                System.out.println();
            }
            else {
                int lcp = lcp(i);
                System.out.printf("%3d %3d %3d %3d %s", i, index, lcp, rank, ith);
                System.out.println();
            }
        }
    }
    //+END_FOLD }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        SuffixArray sa = new SuffixArray("mississippi");
        //sa.debug();
        Assert.assertEquals(11, sa.length());
        Assert.assertEquals(10, sa.index(0));
        Assert.assertEquals(9, sa.index(5));
        Assert.assertEquals("i", sa.select(0));
        Assert.assertEquals("pi", sa.select(5));
        Assert.assertEquals(1, sa.lcp(1));
        Assert.assertEquals(4, sa.lcp(3));
        Assert.assertEquals(0, sa.rank("i"));
        Assert.assertEquals(5, sa.rank("pi"));
        Assert.assertEquals(7, sa.rank("random"));
        System.out.println("OK");
    }
    //+END_FOLD }
}
