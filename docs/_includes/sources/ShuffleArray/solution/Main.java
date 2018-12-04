import java.util.Arrays;

// Knuth's shuffle O(n)
public class Main {
    public static Object[] shuffle(Object[] a) {
        int n = a.length;
        for (int i = 0; i < n; i++) {
            // choose uniformly [0, i]
            int r = (int) Math.random() * (i + 1);
            Object tmp = a[i];
            a[i] = a[r];
            a[r] = tmp;
        }
        return a;
    }

    public static void main(String[] args) {
        try {
            for (int i = 0; i < 10; i++) {
                assert !Arrays.equals(new Integer[] {1,2,3,4,5},
                                      shuffle(new Integer[] {1,2,3,4,5}));
            }
            System.out.println("OK");
        } catch (Throwable e) {
            System.out.println("FAIL");
            e.printStackTrace();
        }
    }
}
