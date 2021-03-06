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

public class RWTrieMapLongestPrefixOf<Value> extends RWTrieMap<Value> {
    public String longestPrefixOf(String s) {
        //+BEGIN_SOLUTION
        int length = search(root, s, 0, 0);
        return s.substring(0, length);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private int search(Node x, String s, int d, int length) {
        if (x == null) return length;
        if (x.val != null) length = d;
        if (d == s.length()) return length;
        char c = s.charAt(d);
        return search(x.next[c], s, d+1, length);
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/she-sells.txt
        String[] keys = new String[] {
            "she", "sells", "sea", "shells", "by", "the", "sea", "shore"
        };
        RWTrieMapLongestPrefixOf<Integer> trie = new RWTrieMapLongestPrefixOf<>();
        int id = 0;
        for (String key : keys) {
            trie.put(key, id++);
        }
        //System.out.println(trie.toDot());
        Assert.assertEquals("she",    trie.longestPrefixOf("she"));
        Assert.assertEquals("she",    trie.longestPrefixOf("shell"));
        Assert.assertEquals("shells", trie.longestPrefixOf("shellsort"));
        Assert.assertEquals("",       trie.longestPrefixOf(""));
        Assert.assertEquals("",       trie.longestPrefixOf("abc"));
        System.out.println("OK");
    }
    //+END_FOLD }
}
