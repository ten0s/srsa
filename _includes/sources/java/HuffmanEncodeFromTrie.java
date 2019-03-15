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

public class HuffmanEncodeFromTrie extends Huffman {
    public static String encode(Node root, String text) {
        //+BEGIN_SOLUTION
        StringBuilder sb = new StringBuilder();
        String[] st = codes(root);
        int n = text.length();
        for (int i = 0; i < n; i++) {
            sb.append(st[text.charAt(i)]);
        }
        return sb.toString();
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private static String[] codes(Node root) {
        String[] st = new String[R];
        codes(root, "", st);
        return st;
    }

    private static void codes(Node x, String prefix, String[] st) {
        if (x.isLeaf()) {
            st[x.ch] = prefix;
        } else {
            codes(x.left , prefix + '0', st);
            codes(x.right, prefix + '1', st);
        }
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Node trie = trieFromText("abracadabra");
        //System.out.println(toDot(trie));
        Assert.assertEquals("01011001110011110101100", encode(trie, "abracadabra"));
        System.out.println("OK");
    }
    //+END_FOLD }
}
