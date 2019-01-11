import java.lang.reflect.Array;
import java.util.Iterator;

public class HashSet<Key> implements Iterable<Key> {
    private final static int MIN_CAPACITY = 31;
    private class Node {
        Key key;
        Node next;

        public Node(Key key, Node next) {
            this.key = key;
            this.next = next;
        }
    }

    private Node[] a;
    private int m;
    private int n;

    public HashSet() {
        this(MIN_CAPACITY);
    }

    @SuppressWarnings("unchecked")
    private HashSet(int m) {
        this.m = m;
        this.a = (Node[]) Array.newInstance(Node.class, m);
    }

    private int hash(Key key) {
        return (key.hashCode() & 0x7fffffff) % m;
    }

    private void resize(int capacity) {
        HashSet<Key> t = new HashSet<>(capacity);
        for (int i = 0; i < m; i++) {
            for (Node x = a[i]; x != null; x = x.next) {
                t.add(x.key);
            }
        }
        this.a = t.a;
        this.m = t.m;
    }

    public boolean contains(Key key) {
        int i = hash(key);
        for (Node x = a[i]; x != null; x = x.next) {
            if (key.equals(x.key)) {
                return true;
            }
        }
        return false;
    }

    public void add(Key key) {
        int i = hash(key);
        for (Node x = a[i]; x != null; x = x.next) {
            if (key.equals(x.key)) {
                return;
            }
        }
        a[i] = new Node(key, a[i]);
        n++;
        if (n >= 8*m) resize(Prime.nextPrime(2*m));
    }

    public void delete(Key key) {
        int i = hash(key);
        Node x = a[i];
        if (x == null) return;
        if (key.equals(x.key)) {
            a[i] = x.next;
            n--;
        } else {
            for (; x.next != null; x = x.next) {
                if (key.equals(x.next.key)) {
                    x.next = x.next.next;
                    n--;
                    break;
                }
            }
        }
        if (n <= 2*m) resize(Math.max(MIN_CAPACITY, Prime.nextPrime(m/2)));
    }

    public boolean isEmpty() {
        return size() == 0;
    }

    public int size() {
        return n;
    }

    public Iterator<Key> iterator() {
        Queue<Key> queue = new Queue<>();
        for (int i = 0; i < m; i++) {
            for (Node x = a[i]; x != null; x = x.next) {
                queue.enqueue(x.key);
            }
        }
        return queue.iterator();
    }

    public static void main(String[] args) throws Throwable {
        HashSet<Integer> s = new HashSet<>();

        Assert.assertEquals(0, s.size());
        Assert.assertTrue(s.isEmpty());
        Assert.assertTrue(Prime.isPrime(s.m));
        Assert.assertFalse(s.contains(1));

        for (int i = 0; i < 1000; i++) {
            s.add(i);
            Assert.assertTrue(s.contains(i));
        }
        Assert.assertEquals(1000, s.size());
        Assert.assertFalse(s.isEmpty());
        Assert.assertTrue(Prime.isPrime(s.m));
        Assert.assertFalse(s.contains(1001));

        for (int i = 0; i < 1000; i++) {
            s.delete(i);
        }
        Assert.assertEquals(0, s.size());
        Assert.assertTrue(s.isEmpty());
        Assert.assertTrue(Prime.isPrime(s.m));
        Assert.assertFalse(s.contains(1));

        System.out.println("OK");
    }
}
