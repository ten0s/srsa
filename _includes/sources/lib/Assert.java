import java.util.Arrays;

public class Assert {
    public static void assertArrayEquals(int[] exp, int[] act) throws Throwable {
        if (!Arrays.equals(exp, act)) {
            System.out.println("Expected: " + ArrayUtil.toString(exp) +
                               ", but saw: " + ArrayUtil.toString(act));
            throw new Throwable();
        }
    }
}
