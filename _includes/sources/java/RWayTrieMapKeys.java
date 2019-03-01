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

public class RWayTrieMapKeys<Value> extends RWayTrieMap<Value> {
    public Iterable<String> keys() {
        //+BEGIN_SOLUTION
        Deque<String> queue = new ArrayDeque<>();
        collect(root, "", queue);
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
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/she-sells.txt
        String[] keys = new String[] {
            "she", "sells", "sea", "shells", "by", "the", "sea", "shore"
        };
        RWayTrieMapKeys<Integer> trie = new RWayTrieMapKeys<>();
        Assert.assertArrayEquals(new String[] {},
                                 ArrayUtil.toArray(trie.keys()));

        int id = 0;
        for (String key : keys) {
            trie.put(key, id++);
        }
        //System.out.println(trie.toDot());
        Assert.assertArrayEquals(ArrayUtil.usort(keys),
                                 ArrayUtil.toArray(trie.keys()));
        System.out.println("OK");
    }
    //+END_FOLD }
}
