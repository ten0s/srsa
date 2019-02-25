public class ArrayOfStringsMSDSort {
    public static String[] sort(String[] a) {
        //+BEGIN_SOLUTION
        int N = a.length;
        String[] aux = new String[N];
        return sort(a, aux, 0, N-1, 0);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    public static String[] sort(String[] a, String[] aux, int lo, int hi, int d) {
        if (lo >= hi) return a;

        int R = 256; // extended ASCII
        int[] count = new int[R+2];

        for (int i = lo; i <= hi; i++)
            count[charAt(a[i], d)+2]++;

        for (int r = 0; r < R+1; r++)
            count[r+1] += count[r];

        for (int i = lo; i <= hi; i++)
            aux[count[charAt(a[i], d)+1]++] = a[i];

        for (int i = lo; i <= hi; i++)
            a[i] = aux[i-lo];

        for (int r = 0; r < R; r++)
            sort(a, aux, lo + count[r], lo + count[r+1]-1, d+1);

        return a;
    }

    private static int charAt(String s, int d) {
        return d < s.length() ? s.charAt(d) : -1;
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        String[] a = new String[] {
            "she", "sells", "seashells", "by", "the", "sea",
            "shore", "the", "shells", "she", "sells", "are",
            "surely", "seashells"
        };
        String[] exp = new String[] {
            "are", "by", "sea", "seashells", "seashells",
            "sells", "sells", "she", "she", "shells",
            "shore", "surely", "the", "the"
        };
        Assert.assertArrayEquals(exp, sort(a));
        System.out.println("OK");
    }
    //+END_FOLD }
}
