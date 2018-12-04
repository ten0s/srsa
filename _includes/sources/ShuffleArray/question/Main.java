import java.util.Arrays;

public class Main {
    public static Object[] shuffle(Object[] a) {

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
