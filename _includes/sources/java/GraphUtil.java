public class GraphUtil {
    public static String pathToString(Iterable<Integer> path) {
        String s = "";
        for (int x : path) {
            if (s.equals("")) {
                s = "" + x;
            } else {
                s += "-" + x;
            }
        }
        return s;
    }
}
