import java.lang.reflect.Array;

/*
public class Prime {
    public static int nextPrime(int n);
}
*/

public class HashTable<Key , Value> {
    private final static int MIN_CAPACITY = 31;
    private class Node {
        Key key;
        Value val;
        Node next;

        public Node(Key key, Value val, Node next) {
            this.key = key;
            this.val = val;
            this.next = next;
        }
    }

    private Node[] a; // buckets
    private int m;    // buckets count
    private int n;    // items count

    public HashTable() {
        this(MIN_CAPACITY);
    }

    @SuppressWarnings("unchecked")
    private HashTable(int m) {
        this.m = m;
        this.a = (Node[]) Array.newInstance(Node.class, m);
    }

    private int hash(Key key) {
        // SOLUTION_BEGIN
        return (key.hashCode() & 0x7fffffff) % m;
        // SOLUTION_END
    }

    private void resize(int capacity) {
        // SOLUTION_BEGIN
        HashTable<Key, Value> t = new HashTable<>(capacity);
        for (int i = 0; i < m; i++) {
            for (Node x = a[i]; x != null; x = x.next) {
                t.put(x.key, x.val);
            }
        }
        this.a = t.a;
        this.m = t.m;
        // SOLUTION_END
    }

    public Value get(Key key) {
        // SOLUTION_BEGIN
        int i = hash(key);
        for (Node x = a[i]; x != null; x = x.next) {
            if (key.equals(x.key)) {
                return x.val;
            }
        }
        return null;
        // SOLUTION_END
    }

    public void put(Key key, Value val) {
        // SOLUTION_BEGIN
        int i = hash(key);
        for (Node x = a[i]; x != null; x = x.next) {
            if (key.equals(x.key)) {
                x.val = val;
                return;
            }
        }
        a[i] = new Node(key, val, a[i]);
        n++;
        if (n >= 8*m) resize(Prime.nextPrime(2*m));
        // SOLUTION_END
    }

    public void delete(Key key) {
        // SOLUTION_BEGIN
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
        // SOLUTION_END
    }

    public boolean isEmpty() {
        // SOLUTION_BEGIN
        return size() == 0;
        // SOLUTION_END
    }

    public int size() {
        // SOLUTION_BEGIN
        return n;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        HashTable<Integer, Integer> t = new HashTable<>();
        Assert.assertTrue(t.isEmpty());
        Assert.assertEquals(0, t.size());
        Assert.assertNull(t.get(1));
        for (int i = 0; i < 1000; i++) {
            t.put(i, i^2);
        }
        Assert.assertFalse(t.isEmpty());
        Assert.assertEquals(1000, t.size());
        for (int i = 0; i < 1000; i++) {
            Assert.assertEquals(i^2, t.get(i));
        }
        Assert.assertNull(t.get(1001));
        for (int i = 0; i < 1000; i++) {
            t.delete(i);
        }
        Assert.assertTrue(t.isEmpty());
        Assert.assertEquals(0, t.size());
        Assert.assertNull(t.get(1));
        System.out.println("OK");
    }
}
