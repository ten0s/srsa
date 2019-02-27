import java.util.NoSuchElementException;
import java.util.Collections;
import java.util.Iterator;
import java.util.Deque;
import java.util.ArrayDeque;

public class BSTMap<Key extends Comparable<Key>, Value> {
    protected Node root;

    protected class Node {
        Key key;
        Value val;
        Node left;
        Node right;
        int size;

        public Node(Key key, Value val) {
            this.key = key;
            this.val = val;
            this.size = 1;
        }
    }

    public Value get(Key key) {
        return get(key, root);
    }

    private Value get(Key key, Node x) {
        if (x == null) return null;
        int cmp = key.compareTo(x.key);
        if      (cmp < 0) return get(key, x.left);
        else if (cmp > 0) return get(key, x.right);
        else              return x.val;
    }

    public void put(Key key, Value val) {
        root = put(key, val, root);
    }

    private Node put(Key key, Value val, Node x) {
        if (x == null) return new Node(key, val);
        int cmp = key.compareTo(x.key);
        if      (cmp < 0) x.left  = put(key, val, x.left);
        else if (cmp > 0) x.right = put(key, val, x.right);
        else              x.val = val;
        x.size = 1 + size(x.left) + size(x.right);
        return x;
    }

    public void remove(Key key) {
        root = remove(key, root);
    }

    private Node remove(Key key, Node x) {
        if (x == null) return null;
        int cmp = key.compareTo(x.key);
        if      (cmp < 0) x.left = remove(key, x.left);
        else if (cmp > 0) x.right = remove(key, x.right);
        else {
            if (x.left == null) return x.right;
            if (x.right == null) return x.left;
            Node t = x;
            if (Math.random() < 0.5) {
                x = max(t.left);
                x.left = removeMax(t.left);
                x.right = t.right;
            } else {
                x = min(t.right);
                x.right = removeMin(t.right);
                x.left = t.left;
            }
        }
        x.size = 1 + size(x.left) + size(x.right);
        return x;
    }

    public boolean contains(Key key) {
        return get(key) != null;
    }

    public boolean isEmpty() {
        return size() == 0;
    }

    public int size() {
        return size(root);
    }

    protected int size(Node x) {
        if (x == null) return 0;
        return x.size;
    }

    public int height() {
        return height(root);
    }

    private int height(Node x) {
        if (x == null) return 0;
        return 1 + Math.max(height(x.left), height(x.right));
    }

    public Key min() {
        if (isEmpty()) throw new NoSuchElementException();
        return min(root).key;
    }

    protected Node min(Node x) {
        if (x.left == null) return x;
        return min(x.left);
    }

    public Key max() {
        if (isEmpty()) throw new NoSuchElementException();
        return max(root).key;
    }

    protected Node max(Node x) {
        if (x.right == null) return x;
        return max(x.right);
    }

    public void removeMin() {
        if (isEmpty()) throw new NoSuchElementException();
        root = removeMin(root);
    }

    protected Node removeMin(Node x) {
        if (x.left == null) return x.right;
        x.left = removeMin(x.left);
        x.size = 1 + size(x.left) + size(x.right);
        return x;
    }

    public void removeMax() {
        if (isEmpty()) throw new NoSuchElementException();
        root = removeMax(root);
    }

    protected Node removeMax(Node x) {
        if (x.right == null) return x.left;
        x.right = removeMax(x.right);
        x.size = 1 + size(x.left) + size(x.right);
        return x;
    }

    public int rank(Key key) {
        return rank(key, root);
    }

    private int rank(Key key, Node x) {
        if (x == null) return 0;
        int cmp = key.compareTo(x.key);
        if      (cmp < 0) return rank(key, x.left);
        else if (cmp > 0) return 1 + size(x.left) + rank(key, x.right);
        else              return size(x.left);
    }

    public Key select(int k) {
        if (k < 0 || k >= size())
            throw new IllegalArgumentException();
        return select(k, root).key;
    }

    private Node select(int k, Node x) {
        if (x == null) return null;
        int t = size(x.left);
        if      (t > k) return select(k, x.left);
        else if (t < k) return select(k-t-1, x.right);
        else            return x;
    }

    public Key floor(Key key) {
        Node x = floor(key, root);
        if (x == null) throw new NoSuchElementException();
        return x.key;
    }

    private Node floor(Key key, Node x) {
        if (x == null) return null;
        int cmp = key.compareTo(x.key);
        if (cmp == 0) return x;
        if (cmp < 0) return floor(key, x.left);
        Node t = floor(key, x.right);
        if (t != null) return t;
        else           return x;
    }

    public Key ceiling(Key key) {
        Node x = ceiling(key, root);
        if (x == null) throw new NoSuchElementException();
        return x.key;
    }

    private Node ceiling(Key key, Node x) {
        if (x == null) return null;
        int cmp = key.compareTo(x.key);
        if (cmp == 0) return x;
        if (cmp > 0) return ceiling(key, x.right);
        Node t = ceiling(key, x.left);
        if (t != null) return t;
        else           return x;
    }

    public Iterable<Key> keys() {
        if (isEmpty())
            return new Iterable<Key>() {
                public Iterator<Key> iterator() {
                    return Collections.emptyIterator();
                }
            };
        return keys(min(), max());
    }

    public Iterable<Key> keys(Key lo, Key hi) {
        Deque<Key> queue = new ArrayDeque<>();
        keys(queue, lo, hi, root);
        return queue;
    }

    private void keys(Deque<Key> queue, Key lo, Key hi, Node x) {
        if (x == null) return;
        int cmpLo = lo.compareTo(x.key);
        int cmpHi = hi.compareTo(x.key);
        if (cmpLo < 0) keys(queue, lo, hi, x.left);
        if (cmpLo <= 0 && cmpHi >= 0) queue.add(x.key);
        if (cmpHi > 0) keys(queue, lo, hi, x.right);
    }

    public String toDot() {
        // credits
        // https://gist.github.com/kstwrt/8591183
        // https://eli.thegreenplace.net/2009/11/23/visualizing-binary-trees-with-graphviz
        StringBuilder sb = new StringBuilder();
        sb.append("digraph {");
        sb.append(System.lineSeparator());
        toDot(root, 0, sb);
        sb.append("}");
        sb.append(System.lineSeparator());
        return sb.toString();
    }

    private int toDot(Node node, int id, StringBuilder sb) {
        if (node == null) {
            sb.append("  " + id + " " + attrs(point()) + ";");
            sb.append(System.lineSeparator());
            return id+1;
        }
        int id2 = toDot(node.left, id+1, sb);
        int id3 = toDot(node.right, id2, sb);
        sb.append("  " + id + " " + attrs(label(node.key + "/" + node.val)) + ";");
        sb.append(System.lineSeparator());
        sb.append("  " + id + " -> " + (id+1) + ";");
        sb.append(System.lineSeparator());
        sb.append("  " + id + " -> " + id2  + ";");
        sb.append(System.lineSeparator());
        return id3+1;
    }

    private String attrs(String attr) {
        return "[" + attr + "]";
    }

    private String label(Object o) {
        return "label=" + Character.toString('"') + o + Character.toString('"');
    }

    private String point() {
        return "shape=point";
    }
}
