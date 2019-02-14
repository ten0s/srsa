import java.util.Arrays;

public class Assert {
    public static void assertTrue(boolean cond) throws Throwable {
        assertTrue("", cond);
    }

    public static void assertTrue(String msg, boolean cond) throws Throwable {
        if (!cond) {
            System.err.println("Expected true");
            if (msg != null && !msg.equals("")) {
                System.err.println(msg);
            }
            throw new Throwable();
        }
    }

    public static void assertFalse(boolean cond) throws Throwable {
        assertFalse("", cond);
    }

    public static void assertFalse(String msg, boolean cond) throws Throwable {
        if (cond) {
            System.err.println("Expected false");
            if (msg != null && !msg.equals("")) {
                System.err.println(msg);
            }
            throw new Throwable();
        }
    }

    public static void assertNull(Object obj) throws Throwable {
        assertNull("", obj);
    }

    public static void assertNull(String msg, Object obj) throws Throwable {
        if (obj != null) {
            System.err.println("Expected null");
            if (msg != null && !msg.equals("")) {
                System.err.println(msg);
            }
            throw new Throwable();
        }
    }

    public static void assertNotNull(Object obj) throws Throwable {
        assertNotNull("", obj);
    }

    public static void assertNotNull(String msg, Object obj) throws Throwable {
        if (obj == null) {
            System.err.println("Expected not null");
            if (msg != null && !msg.equals("")) {
                System.err.println(msg);
            }
            throw new Throwable();
        }
    }

    public static void assertEquals(long exp, long act) throws Throwable {
        assertEquals("", exp, act);
    }

    public static void assertEquals(String msg, long exp, long act) throws Throwable {
        if (exp != act) {
            System.err.println("Expected: " + exp + ", but saw: " + act);
            if (msg != null && !msg.equals("")) {
                System.err.println(msg);
            }
            throw new Throwable();
        }
    }

    public static void assertEquals(double exp, double act, double eps) throws Throwable {
        assertEquals("", exp, act, eps);
    }

    public static void assertEquals(String msg, double exp, double act, double eps) throws Throwable {
        if (Math.abs(exp - act) > eps) {
            System.err.println("Expected: " + exp + ", but saw: " + act);
            if (msg != null && !msg.equals("")) {
                System.err.println(msg);
            }
            throw new Throwable();
        }
    }

    public static void assertEquals(Object exp, Object act) throws Throwable {
        assertEquals("", exp, act);
    }

    public static void assertEquals(String msg, Object exp, Object act) throws Throwable {
        if (!exp.equals(act)) {
            System.err.println("Expected: " + exp + ", but saw: " + act);
            if (msg != null && !msg.equals("")) {
                System.err.println(msg);
            }
            throw new Throwable();
        }
    }

    public static void assertArrayEquals(int[] exp, int[] act) throws Throwable {
        assertArrayEquals("", exp, act);
    }

    public static void assertArrayEquals(String msg, int[] exp, int[] act) throws Throwable {
        if (!Arrays.equals(exp, act)) {
            System.out.println("Expected: " + ArrayUtil.toString(exp) +
                               ", but saw: " + ArrayUtil.toString(act));
            if (msg != null && !msg.equals("")) {
                System.err.println(msg);
            }
            throw new Throwable();
        }
    }

    public static void assertArrayEquals(Object[] exp, Object[] act) throws Throwable {
        assertArrayEquals("", exp, act);
    }

    public static void assertArrayEquals(String msg, Object[] exp, Object[] act) throws Throwable {
        if (!Arrays.equals(exp, act)) {
            System.out.println("Expected: " + ArrayUtil.toString(exp) +
                               ", but saw: " + ArrayUtil.toString(act));
            if (msg != null && !msg.equals("")) {
                System.err.println(msg);
            }
            throw new Throwable();
        }
    }
}
