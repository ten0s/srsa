import java.util.Arrays;

public class Assert {
    public static void assertTrue(boolean condition) throws Throwable {
        if (!condition) {
            System.err.println("Expected true");
            throw new Throwable();
        }
    }

    public static void assertFalse(boolean condition) throws Throwable {
        if (condition) {
            System.err.println("Expected false");
            throw new Throwable();
        }
    }

    public static void assertNull(Object obj) throws Throwable {
        if (obj != null) {
            System.err.println("Expected null");
            throw new Throwable();
        }
    }

    public static void assertNotNull(Object obj) throws Throwable {
        if (obj == null) {
            System.err.println("Expected not null");
            throw new Throwable();
        }
    }

    public static void assertEquals(Integer exp, int act) throws Throwable {
        assertEquals((int)exp, act);
    }

    public static void assertEquals(int exp, Integer act) throws Throwable {
        assertEquals(exp, (int)act);
    }

    public static void assertEquals(int exp, int act) throws Throwable {
        if (exp != act) {
            System.err.println("Expected: " + exp + ", but saw: " + act);
            throw new Throwable();
        }
    }

    public static void assertEquals(String exp, String act) throws Throwable {
        if (!exp.equals(act)) {
            System.err.println("Expected: " + exp + ", but saw: " + act);
            throw new Throwable();
        }
    }

    public static void assertEquals(Object exp, Object act) throws Throwable {
        if (!exp.equals(act)) {
            System.err.println("Expected: " + exp + ", but saw: " + act);
            throw new Throwable();
        }
    }

    public static void assertArrayEquals(int[] exp, int[] act) throws Throwable {
        if (!Arrays.equals(exp, act)) {
            System.out.println("Expected: " + ArrayUtil.toString(exp) +
                               ", but saw: " + ArrayUtil.toString(act));
            throw new Throwable();
        }
    }

    public static void assertArrayEquals(Object[] exp, Object[] act) throws Throwable {
        if (!Arrays.equals(exp, act)) {
            System.out.println("Expected: " + ArrayUtil.toString(exp) +
                               ", but saw: " + ArrayUtil.toString(act));
            throw new Throwable();
        }
    }
}
