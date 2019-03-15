import java.util.*;

/*
public class Huffman {
    protected static final int R = 256; // extended ASCII
    protected static class Node implements Comparable<Node> {
        char ch;
        int freq;
        final Node left, right;

        public Node(char ch, int freq, Node left, Node right);
        public boolean isLeaf();
    }
}
*/

public class HuffmanTrieFromDump extends Huffman {
    public static Node trieFromDump(String dump) {
        //+BEGIN_SOLUTION
        Deque<Character> q = new ArrayDeque<>();
        int n = dump.length();
        for (int i = 0; i < n; i++) {
            q.add(dump.charAt(i));
        }
        return undump(q);
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private static Node undump(Deque<Character> q) {
        char c = q.remove();
        if (c == '1') {
            return new Node(q.remove(), 0, null, null);
        }
        Node l = undump(q);
        Node r = undump(q);
        return new Node('\0', 0, l, r);
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Node trie = trieFromDump("01a01b01r01c1d");
        //System.out.println(toDot(trie));
        Assert.assertEquals("01a01b01r01c1d", toDump(trie));
        System.out.println("OK");
    }
    //+END_FOLD }
}
