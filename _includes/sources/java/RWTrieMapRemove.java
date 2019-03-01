/*
public class RWTrieMap<Value> {
    protected final static int R = 256; // extended ASCII
    protected Node root = new Node();
    protected int size;

    protected static class Node {
        Object val;
        Node[] next = new Node[R];
    }
}
*/

public class RWTrieMapRemove<Value> extends RWTrieMap<Value> {
    public void remove(String key) {
        //+BEGIN_SOLUTION
        remove(root, key, 0);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private Node remove(Node x, String key, int d) {
        if (x == null) return null;
        if (d == key.length()) {
            if (x.val != null) {
                size--;
                x.val = null;
            }
        } else {
            char c = key.charAt(d);
            x.next[c] = remove(x.next[c], key, d+1);
        }
        if (x.val != null) return x;
        for (char c = 0; c < R; c++) {
            if (x.next[c] != null) return x;
        }
        return null;
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        // data/she-sells.txt
        String[] keys = new String[] {
            "she", "sells", "sea", "shells", "by", "the", "sea", "shore"
        };
        RWTrieMapRemove<Integer> trie = new RWTrieMapRemove<>();
        int id = 0;
        for (String key : keys) {
            trie.put(key, id++);
        }
        //System.out.println(trie.toDot());
        Assert.assertEquals(7, trie.size());

        Assert.assertEquals(0, (int)trie.get("she"));
        trie.remove("she");
        Assert.assertNull(trie.get("she"));
        Assert.assertEquals(6, trie.size());

        Assert.assertEquals(1, (int)trie.get("sells"));
        trie.remove("sells");
        Assert.assertNull(trie.get("sells"));
        Assert.assertEquals(5, trie.size());

        Assert.assertEquals(6, (int)trie.get("sea"));
        trie.remove("sea");
        Assert.assertNull(trie.get("sea"));
        Assert.assertEquals(4, trie.size());

        Assert.assertEquals(7, (int)trie.get("shore"));
        trie.remove("shore");
        Assert.assertNull(trie.get("shore"));
        Assert.assertEquals(3, trie.size());

        Assert.assertNull(trie.get("shellfish"));
        trie.remove("shellfish");
        Assert.assertNull(trie.get("shellfish"));

        for (String key : trie.keys()) trie.remove(key);
        Assert.assertEquals(0, trie.size());

        System.out.println("OK");
    }
    //+END_FOLD }
}
