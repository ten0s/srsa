import java.util.*;

/*
public class RWTrieMap<Value> {
    protected final static int R = 256; // extended ASCII
    protected Node root = new Node();

    protected static class Node {
        Object val;
        Node[] next = new Node[R];
    }
}
*/

public class RWTrieMapKeysThatMatch<Value> extends RWTrieMap<Value> {
    public Iterable<String> keysThatMatch(String pattern) {
        //+BEGIN_SOLUTION
        Deque<String> queue = new ArrayDeque<>();
        collect(root, "", pattern, queue);
        return queue;
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
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
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/she-sells.txt
        String[] keys = new String[] {
            "she", "sells", "sea", "shells", "by", "the", "sea", "shore"
        };
        RWTrieMapKeysThatMatch<Integer> trie = new RWTrieMapKeysThatMatch<>();
        int id = 0;
        for (String key : keys) {
            trie.put(key, id++);
        }
        //System.out.println(trie.toDot());
        Assert.assertArrayEquals(new String[] {"she", "the"},
                                 ArrayUtil.toArray(trie.keysThatMatch(".he")));
        Assert.assertArrayEquals(new String[] {},
                                 ArrayUtil.toArray(trie.keysThatMatch("")));
        System.out.println("OK");
    }
    //+END_FOLD }
}
