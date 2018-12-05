public class ArrayUtil {
    public static String toString(int[] a) {
        String s = "[";
        for (int i = 0; i < a.length; i++) {
            s += a[i];
            if (i != a.length-1) {
                s += ",";
            }
        }
        s += "]";
        return s;
    }

    public static String toString(Object[] a) {
        String s = "[";
        for (int i = 0; i < a.length; i++) {
            s += a[i];
            if (i != a.length-1) {
                s += ",";
            }
        }
        s += "]";
        return s;
    }
}
