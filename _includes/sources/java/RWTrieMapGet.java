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

public class RWTrieMapGet<Value> extends RWTrieMap<Value> {
    @SuppressWarnings("unchecked")
    public Value get(String key) {
        //+BEGIN_SOLUTION
        Node x = get(root, key, 0);
        if (x == null) return null;
        return (Value) x.val;
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
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
        RWTrieMapGet<Integer> trie = new RWTrieMapGet<>();
        Assert.assertEquals(0, trie.size());
        Assert.assertNull(trie.get("she"));
        int id = 0;
        for (String key : keys) {
            trie.put(key, id++);
        }
        //System.out.println(trie.toDot());
        Assert.assertEquals(7, trie.size());
        Assert.assertEquals(0, (int)trie.get("she"));
        Assert.assertEquals(1, (int)trie.get("sells"));
        Assert.assertEquals(6, (int)trie.get("sea"));
        Assert.assertEquals(7, (int)trie.get("shore"));
        Assert.assertNull(trie.get("shellfish"));
        System.out.println("OK");
    }
    //+END_FOLD }
}
