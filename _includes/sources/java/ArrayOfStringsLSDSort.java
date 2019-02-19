public class ArrayOfStringsLSDSort {
    public static String[] sort(String[] a, int W) {
        //+BEGIN_SOLUTION
        int N = a.length;
        int R = 256; // extend ASCII alphabet size
        String[] aux = new String[N];

        for (int d = W-1; d >= 0; d--) {
            int[] count = new int[R+1];

            for (int i = 0; i < N; i++)
                count[a[i].charAt(d)+1]++;

            for (int r = 0; r < R; r++)
                count[r+1] += count[r];

            for (int i = 0; i < N; i++)
                aux[count[a[i].charAt(d)]++] = a[i];

            for (int i = 0; i < N; i++) {
                a[i] = aux[i];
            }
        }

        return a;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        String[] a = new String[] {
            "4PGC938", "2IYE230", "3CI0720", "1ICK750",
            "1OHV845", "4JZY524", "1ICK750", "3CI0720",
            "1OHV845", "1OHV845", "2RLA629", "2RLA629",
            "3ATW723"
        };
        int W = a[0].length();
        String[] exp = new String[] {
            "1ICK750", "1ICK750",
            "1OHV845", "1OHV845", "1OHV845",
            "2IYE230",
            "2RLA629", "2RLA629",
            "3ATW723",
            "3CI0720", "3CI0720",
            "4JZY524",
            "4PGC938"
        };
        Assert.assertArrayEquals(exp, sort(a, W));
        System.out.println("OK");
    }
    //+END_FOLD }
}
