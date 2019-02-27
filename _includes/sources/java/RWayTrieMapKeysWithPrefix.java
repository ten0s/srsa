//+BEGIN_SOLUTION
import java.util.Deque;
import java.util.ArrayDeque;
//+END_SOLUTION

/*
public class RWayTrieMap<Value> {
    protected final static int R = 256; // extended ASCII
    protected Node root = new Node();

    protected static class Node {
        Object val;
        Node[] next = new Node[R];
    }
}
*/

public class RWayTrieMapKeysWithPrefix<Value> extends RWayTrieMap<Value> {
    public Iterable<String> keys() {
        //+BEGIN_SOLUTION
        return keysWithPrefix("");
        //+END_SOLUTION
    }

    public Iterable<String> keysWithPrefix(String prefix) {
        //+BEGIN_SOLUTION
        Deque<String> queue = new ArrayDeque<>();
        collect(get(root, prefix, 0), prefix, queue);
        return queue;
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private void collect(Node x, String prefix, Deque<String> queue) {
        if (x == null) return;
        if (x.val != null) queue.add(prefix);
        for (char c = 0; c < R; c++)
            collect(x.next[c], prefix + c, queue);
    }

    private Node get(Node x, String key, int d) {
        if (x == null) return null;
        if (d == key.length()) return x;
        char c = key.charAt(d);
        return get(x.next[c], key, d+1);
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/she-sells.txt
        String[] keys = new String[] {
            "she", "sells", "sea", "shells", "by", "the", "sea", "shore"
        };
        RWayTrieMapKeysWithPrefix<Integer> trie = new RWayTrieMapKeysWithPrefix<>();
        int id = 0;
        for (String key : keys) {
            trie.put(key, id++);
        }
        //System.out.println(trie.toDot());
        Assert.assertArrayEquals(ArrayUtil.usort(keys),
                                 ArrayUtil.toArray(trie.keys()));
        Assert.assertArrayEquals(new String[] {"she", "shells", "shore"},
                                 ArrayUtil.toArray(trie.keysWithPrefix("sh")));
        Assert.assertArrayEquals(new String[] {},
                                 ArrayUtil.toArray(trie.keysWithPrefix("sha")));
        System.out.println("OK");
    }
    //+END_FOLD }
}
