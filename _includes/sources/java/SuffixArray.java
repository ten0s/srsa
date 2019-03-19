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

        private char charAt(int i ) {
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

    public SuffixArray(String text) {
        //+BEGIN_SOLUTION
        int n = text.length();
        suffixes = new Suffix[n];
        for (int i = 0; i < n; i++)
            suffixes[i] = new Suffix(text, i);
        Arrays.sort(suffixes);
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
        int n = Math.min(suffixes[i-1].length(), suffixes[i].length());
        for (int j = 0; j < n; j++)
            if (suffixes[i-1].charAt(j) != suffixes[i].charAt(j))
                return j;
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
    private int compare(String key, Suffix suffix) {
        int n = Math.min(key.length(), suffix.length());
        for (int i = 0; i < n; i++) {
            if (key.charAt(i) < suffix.charAt(i)) return -1;
            if (key.charAt(i) > suffix.charAt(i)) return +1;
        }
        return key.length() - suffix.length();
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        System.out.println("OK");
    }
    //+END_FOLD }
}
