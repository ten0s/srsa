import java.util.Arrays;

public class ShuffleArray {
    public static <T> T[] shuffle(T[] a) {
        // SOLUTION_BEGIN
        // Knuth's shuffle O(n)
        int n = a.length;
        for (int i = 0; i < n; i++) {
            // choose uniformly [0, i]
            int r = (int) (Math.random() * (i + 1));
            T tmp = a[i];
            a[i] = a[r];
            a[r] = tmp;
        }
        return a;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        for (int i = 0; i < 100; i++) {
            Assert.assertFalse(
                Arrays.equals(
                    new Integer[] {1,2,3,4,5,6,7,8,9,10},
                    shuffle(new Integer[] {1,2,3,4,5,6,7,8,9,10})));
        }
        System.out.println("OK");
    }
}
