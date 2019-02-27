import java.util.Deque;
import java.util.ArrayDeque;

public class RWayTrieMap<Value> {
    protected final static int R = 256; // extended ASCII
    protected Node root = new Node();
    protected int size;

    protected static class Node {
        Object val;
        Node[] next = new Node[R];
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public int size() {
        return size;
    }

    public boolean contains(String key) {
        return get(key) != null;
    }

    @SuppressWarnings("unchecked")
    public Value get(String key) {
        Node x = get(root, key, 0);
        if (x == null) return null;
        return (Value) x.val;
    }

    private Node get(Node x, String key, int d) {
        if (x == null) return null;
        if (d == key.length()) return x;
        char c = key.charAt(d);
        return get(x.next[c], key, d+1);
    }

    public void put(String key, Value val) {
        root = put(root, key, val, 0);
    }

    private Node put(Node x, String key, Value val, int d) {
        if (x == null) x = new Node();
        if (d == key.length()) {
            if (x.val == null) size++;
            x.val = val;
        } else {
            char c = key.charAt(d);
            x.next[c] = put(x.next[c], key, val, d+1);
        }
        return x;
    }

    public void remove(String key) {
        root = remove(root, key, 0);
    }

    private Node remove(Node x, String key, int d) {
        if (x == null) return null;
        if (d == key.length()) {
            if (x.val != null) size--;
            x.val = null;
        } else {
            char c = key.charAt(d);
            x.next[c] = remove(x.next[c], key, d+1);
        }
        if (x.val != null) return x;

        for (char c = 0; c < R; c++)
            if (x.next[c] != null) return x;
        return null;
    }

    public Iterable<String> keys() {
        return keysWithPrefix("");
    }

    public Iterable<String> keysWithPrefix(String prefix) {
        Deque<String> queue = new ArrayDeque<>();
        collect(get(root, prefix, 0), prefix, queue);
        return queue;
    }

    private void collect(Node x, String prefix, Deque<String> queue) {
        if (x == null) return;
        if (x.val != null) queue.add(prefix);
        for (char c = 0; c < R; c++)
            collect(x.next[c], prefix + c, queue);
    }

    public Iterable<String> keysThatMatch(String pattern) {
        Deque<String> queue = new ArrayDeque<>();
        collect(root, "", pattern, queue);
        return queue;
    }

    private void collect(Node x, String prefix, String pattern, Deque<String> queue) {
        if (x == null) return;
        int d = prefix.length();
        if (d == pattern.length() && x.val != null) queue.add(prefix);
        if (d == pattern.length()) return;

        char next = pattern.charAt(d);
        for (char c = 0; c < R; c++)
            if (next == '.' || next == c)
                collect(x.next[c], prefix + c, pattern, queue);
    }

    public String longestPrefixOf(String s) {
        int length = search(root, s, 0, 0);
        return s.substring(0, length);
    }

    private int search(Node x, String s, int d, int length) {
        if (x == null) return length;
        if (x.val != null) length = d;
        if (d == s.length()) return length;
        char c = s.charAt(d);
        return search(x.next[c], s, d+1, length);
    }

    public String toDot() {
        StringBuilder sb = new StringBuilder();
        sb.append("graph {");
        sb.append(System.lineSeparator());
        sb.append("  0 [" + label("") + "];");
        sb.append(System.lineSeparator());
        toDot(root, 0, 1, sb);
        sb.append("}");
        sb.append(System.lineSeparator());
        return sb.toString();
    }

    private int toDot(Node x, int pid, int id, StringBuilder sb) {
        if (x == null) return id;
        for (char c = 0; c < R; c++) {
            if (x.next[c] != null) {
                sb.append("  " + id + " " + attrs(label(c), xlabel(x.next[c].val)) + ";");
                sb.append(System.lineSeparator());
                sb.append("  " + pid + " -- " + id + ";");
                sb.append(System.lineSeparator());
                id = toDot(x.next[c], id, id+1, sb);
            }
        }
        return id;
    }

    private String attrs(String label, String xlabel) {
        return "[" + label + (xlabel.equals("") ? "" : " " + xlabel) + "]";
    }

    private String label(Object o) {
        return "label=" + Character.toString('"') + o + Character.toString('"');
    }

    private String xlabel(Object o) {
        if (o != null) {
            return "xlabel=" + Character.toString('"') + o + Character.toString('"');
        } else {
            return "";
        }
    }
}
