import java.util.*;

public class Huffman {
    protected static final int R = 256; // extended ASCII

    protected static class Node implements Comparable<Node> {
        char ch;
        int freq;
        final Node left, right;

        public Node(char ch, int freq, Node left, Node right) {
            this.ch    = ch;
            this.freq  = freq;
            this.left  = left;
            this.right = right;
        }

        public boolean isLeaf() {
            return left == null && right == null;
        }

        public int compareTo(Node that) {
            return this.freq - that.freq;
        }
    }

    public static Node trieFromText(String text) {
        int[] freq = new int[R];
        int n = text.length();
        for (int i = 0; i < n; i++) {
            freq[text.charAt(i)]++;
        }
        PriorityQueue<Node> pq = new PriorityQueue<>();
        for (char c = 0; c < R; c++) {
            if (freq[c] > 0) {
                pq.add(new Node(c, freq[c], null, null));
            }
        }
        while (pq.size() > 1) {
            Node l = pq.remove();
            Node r = pq.remove();
            pq.add(new Node('\0', l.freq + r.freq, l, r));
        }
        return pq.remove();
    }

    public static Node trieFromDump(String dump) {
        Deque<Character> q = new ArrayDeque<>();
        int n = dump.length();
        for (int i = 0; i < n; i++) {
            q.add(dump.charAt(i));
        }
        return undump(q);
    }

    private static Node undump(Deque<Character> q) {
        char c = q.remove();
        if (c == '1') {
            return new Node(q.remove(), 0, null, null);
        }
        Node l = undump(q);
        Node r = undump(q);
        return new Node('\0', 0, l, r);
    }

    public static String toDump(Node root) {
        StringBuilder sb = new StringBuilder();
        dump(root, sb);
        return sb.toString();
    }

    private static void dump(Node x, StringBuilder sb) {
        if (x.isLeaf()) {
            sb.append('1');
            sb.append(x.ch);
        } else {
            sb.append('0');
            dump(x.left, sb);
            dump(x.right, sb);
        }
    }

    public static String encode(Node root, String text) {
        StringBuilder sb = new StringBuilder();
        String[] codes = codes(root);
        int n = text.length();
        for (int i = 0; i < n; i++) {
            sb.append(codes[text.charAt(i)]);
        }
        return sb.toString();
    }

    private static String[] codes(Node root) {
        String[] st = new String[R];
        codes(root, "", st);
        return st;
    }

    private static void codes(Node x, String prefix, String[] st) {
        if (x.isLeaf()) {
            st[x.ch] = prefix;
        } else {
            codes(x.left, prefix + '0', st);
            codes(x.right, prefix + '1', st);
        }
    }

    public static String decode(Node root, String dump) {
        StringBuilder sb = new StringBuilder();
        int n = dump.length();
        int i = 0;
        while (i < n) {
            Node x = root;
            while (!x.isLeaf()) {
                if (dump.charAt(i++) == '0')
                    x = x.left;
                else
                    x = x.right;
            }
            sb.append(x.ch);
        }
        return sb.toString();
    }

    public static String toDot(Node root) {
        StringBuilder sb = new StringBuilder();
        sb.append("graph {");
        sb.append(System.lineSeparator());
        sb.append("  node " + attrs(shape("circle")));
        sb.append(System.lineSeparator());
        toDot(root, 0, sb);
        sb.append("}");
        sb.append(System.lineSeparator());
        return sb.toString();
    }

    private static int toDot(Node x, int id, StringBuilder sb) {
        if (x == null) return id;
        int idLeft  = toDot(x.left , id+1  , sb);
        int idRight = toDot(x.right, idLeft , sb);
        if (x.isLeaf()) {
            sb.append("  " + id + " " + attrs(label(x.ch)));
            sb.append(System.lineSeparator());
        } else {
            sb.append("  " + id + " " + attrs(label(' ')));
            sb.append(System.lineSeparator());
            // link to left subtree
            sb.append("  " + id + " -- " + (id+1) + " " + attrs(label('0')));
            sb.append(System.lineSeparator());
            // link to right subtree
            sb.append("  " + id + " -- " + idLeft + " " + attrs(label('1')));
            sb.append(System.lineSeparator());
        }
        return idRight+1;
    }

    private static String attrs(String... attrs) {
        Deque<String> attrs2 = new ArrayDeque<>();
        for (String attr : attrs) if (attr != null) attrs2.add(attr);

        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (String attr : attrs2) {
            sb.append(attr);
            attrs2.remove();
            if (attrs2.size() > 0) sb.append(", ");
        }
        sb.append("]");
        return sb.toString();
    }

    private static String label(Object o) {
        return "label=" + Character.toString('"') + o + Character.toString('"');
    }

    private static String shape(String s) {
        return "shape=" + Character.toString('"') + s + Character.toString('"');
    }
}
