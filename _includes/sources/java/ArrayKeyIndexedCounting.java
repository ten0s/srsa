public class ArrayKeyIndexedCounting {
    public static int[] sort(int[] a, int R) {
        //+BEGIN_SOLUTION
        int N = a.length;
        int[] aux = new int[N];

        int[] count = new int[R+1];

        for (int i = 0; i < N; i++)
            count[a[i]+1]++;

        for (int r = 0; r < R; r++)
            count[r+1] += count[r];

        for (int i = 0; i < N; i++)
            aux[count[a[i]]++] = a[i];

        for (int i = 0; i < N; i++)
            a[i] = aux[i];

        return a;
        //+END_SOLUTION
    }

    public static void main(String[] args) throws Throwable {
        int R = 6;
        int[] a = new int[] {3,1,4,0,5,2,5,4,3,2,1,0,0,1,2,3,4,5};
        int[] exp = new int[] {0,0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5};
        Assert.assertArrayEquals(exp, sort(a, R));
        System.out.println("OK");
    }
}
