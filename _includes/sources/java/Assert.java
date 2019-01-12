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

    public static void assertEquals(Long exp, long act) throws Throwable {
        assertEquals((long)exp, act);
    }

    public static void assertEquals(long exp, Long act) throws Throwable {
        assertEquals(exp, (long)act);
    }

    public static void assertEquals(long exp, long act) throws Throwable {
        if (exp != act) {
            System.err.println("Expected: " + exp + ", but saw: " + act);
            throw new Throwable();
        }
    }

    public static void assertEquals(Float exp, float act, float eps) throws Throwable {
        assertEquals((float)exp, act, eps);
    }

    public static void assertEquals(float exp, Float act, float eps) throws Throwable {
        assertEquals(exp, (float)act, eps);
    }

    public static void assertEquals(float exp, float act, float eps) throws Throwable {
        if (Math.abs(exp - act) > eps) {
            System.err.println("Expected: " + exp + ", but saw: " + act);
            throw new Throwable();
        }
    }

    public static void assertEquals(Double exp, double act, double eps) throws Throwable {
        assertEquals((double)exp, act, eps);
    }

    public static void assertEquals(double exp, Double act, double eps) throws Throwable {
        assertEquals(exp, (double)act, eps);
    }

    public static void assertEquals(double exp, double act, double eps) throws Throwable {
        if (Math.abs(exp - act) > eps) {
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

    public static void assertArrayEquals(Integer[] exp, int[] act) throws Throwable {
        int[] e = new int[exp.length];
        for (int i = 0; i < exp.length; i++) {
            e[i] = exp[i];
        }
        assertArrayEquals(e, act);
    }

    public static void assertArrayEquals(int[] exp, Integer[] act) throws Throwable {
        int[] a = new int[act.length];
        for (int i = 0; i < act.length; i++) {
            a[i] = exp[i];
        }
        assertArrayEquals(exp, a);
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
