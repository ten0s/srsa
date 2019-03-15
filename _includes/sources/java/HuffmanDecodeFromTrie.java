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

public class HuffmanDecodeFromTrie extends Huffman {
    public static String decode(Node root, String dump) {
        //+BEGIN_SOLUTION
        StringBuilder sb = new StringBuilder();
        int n = dump.length();
        int i = 0;
        while (i < n) {
            Node x = root;
            while (!x.isLeaf()) {
                if (dump.charAt(i++) == '0')
                    x = x.left;
                else
                    x = x.right;
            }
            sb.append(x.ch);
        }
        return sb.toString();
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Node trie = trieFromText("abracadabra");
        //System.out.println(toDot(trie));
        Assert.assertEquals("abracadabra", decode(trie, "01011001110011110101100"));
        System.out.println("OK");
    }
    //+END_FOLD }
}
