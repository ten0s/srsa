/*
public class RWayTrieMap<Value> {
    protected final static int R = 256; // extended ASCII
    protected Node root = new Node();
    protected int size;

    protected static class Node {
        Object val;
        Node[] next = new Node[R];
    }
}
*/

public class RWayTrieMapPut<Value> extends RWayTrieMap<Value> {
    public void put(String key, Value val) {
        //+BEGIN_SOLUTION
        root = put(root, key, val, 0);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private Node put(Node x, String key, Value val, int d) {
        if (x == null) x = new Node();
        if (d == key.length()) {
            if (x.val == null) size++;
            x.val = val;
        } else {
            char c = key.charAt(d);
            x.next[c] = put(x.next[c], key, val, d+1);
        }
        return x;
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/she-sells.txt
        String[] keys = new String[] {
            "she", "sells", "sea", "shells", "by", "the", "sea", "shore"
        };
        RWayTrieMapPut<Integer> trie = new RWayTrieMapPut<>();
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
