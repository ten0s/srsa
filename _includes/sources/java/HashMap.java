import java.lang.reflect.Array;

/*
public class Prime {
    public static int nextPrime(int n);
}
*/

public class HashMap<Key , Value> {
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

    public HashMap() {
        this(MIN_CAPACITY);
    }

    @SuppressWarnings("unchecked")
    private HashMap(int m) {
        this.m = m;
        this.a = (Node[]) Array.newInstance(Node.class, m);
    }

    private int hash(Key key) {
        //+BEGIN_SOLUTION
        return (key.hashCode() & 0x7fffffff) % m;
        //+END_SOLUTION
    }

    private void resize(int capacity) {
        //+BEGIN_SOLUTION
        if (this.m == capacity) return;
        HashMap<Key, Value> hm = new HashMap<>(capacity);
        for (int i = 0; i < m; i++) {
            for (Node x = a[i]; x != null; x = x.next) {
                hm.put(x.key, x.val);
            }
        }
        this.a = hm.a;
        this.m = hm.m;
        //+END_SOLUTION
    }

    public Value get(Key key) {
        //+BEGIN_SOLUTION
        int i = hash(key);
        for (Node x = a[i]; x != null; x = x.next) {
            if (key.equals(x.key)) {
                return x.val;
            }
        }
        return null;
        //+END_SOLUTION
    }

    public void put(Key key, Value val) {
        //+BEGIN_SOLUTION
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
        //+END_SOLUTION
    }

    public void delete(Key key) {
        //+BEGIN_SOLUTION
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
        //+END_SOLUTION
    }

    public boolean isEmpty() {
        return size() == 0;
    }

    public int size() {
        return n;
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        HashMap<Integer, Integer> hm = new HashMap<>();

        Assert.assertEquals(0, hm.size());
        Assert.assertTrue(hm.isEmpty());
        Assert.assertEquals(MIN_CAPACITY, hm.m);
        Assert.assertNull(hm.get(1));

        for (int i = 0; i < 1000; i++) {
            hm.put(i, i);
            Assert.assertEquals(i, (int)hm.get(i));
            hm.put(i, i*i);
            Assert.assertEquals(i*i, (int)hm.get(i));
        }
        Assert.assertEquals(1000, hm.size());
        Assert.assertFalse(hm.isEmpty());
        Assert.assertTrue(hm.m > MIN_CAPACITY);
        Assert.assertTrue(Prime.isPrime(hm.m));
        Assert.assertNull(hm.get(1001));

        for (int i = 0; i < 1000; i++) {
            hm.delete(i);
        }
        Assert.assertEquals(0, hm.size());
        Assert.assertTrue(hm.isEmpty());
        Assert.assertEquals(MIN_CAPACITY, hm.m);
        Assert.assertNull(hm.get(1));

        System.out.println("OK");
    }
    //+END_FOLD }
}
