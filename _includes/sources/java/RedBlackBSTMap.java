import java.util.NoSuchElementException;
import java.util.Collections;
import java.util.Iterator;

public class RedBlackBSTMap<Key extends Comparable<Key>, Value> extends BSTMap<Key, Value> {
    protected static final boolean RED   = true;
    protected static final boolean BLACK = false;

    public void put(Key key, Value val) {
        root = put(root, key, val);
        root.color = BLACK;
    }

    private Node put(Node h, Key key, Value val) {
        if (h == null) return new Node(key, val, RED);
        int cmp = key.compareTo(h.key);
        if      (cmp < 0) h.left  = put(h.left, key, val);
        else if (cmp > 0) h.right = put(h.right, key, val);
        else              h.val = val;

        if (!isRed(h.left) && isRed(h.right))     h = rotateLeft(h);
        if ( isRed(h.left) && isRed(h.left.left)) h = rotateRight(h);
        if ( isRed(h.left) && isRed(h.right))     flipColors(h);

        h.size = 1 + size(h.left) + size(h.right);
        return h;
    }

    private boolean isRed(Node x) {
        // null links are black
        if (x == null) return false;
        return x.color == RED;
    }

    private Node rotateLeft(Node h) {
        assert isRed(h.right);
        Node x = h.right;
        h.right = x.left;
        x.left = h;
        x.color = h.color;
        h.color = RED;
        x.size = h.size;
        h.size = 1 + size(h.left) + size(h.right);
        return x;
    }

    private Node rotateRight(Node h) {
        assert isRed(h.left);
        Node x = h.left;
        h.left = x.right;
        x.right = h;
        x.color = h.color;
        h.color = RED;
        x.size = h.size;
        h.size = 1 + size(h.left) + size(h.right);
        return x;
    }

    private void flipColors(Node h) {
        assert !isRed(h);
        assert isRed(h.left);
        assert isRed(h.right);
        h.color = RED;
        h.left.color = BLACK;
        h.right.color = BLACK;
    }

    // NB: deleteMin(), deleteMax(), delete(Key key) should be re-implemented
}
