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

public class HuffmanTrieToDump extends Huffman {
    public static String toDump(Node root) {
        //+BEGIN_SOLUTION
        StringBuilder sb = new StringBuilder();
        dump(root, sb);
        return sb.toString();
        //+END_SOLUTION
    }

    //+BEGIN_SOLUTION
    private static void dump(Node x, StringBuilder sb) {
        if (x.isLeaf()) {
            sb.append('1').append(x.ch);
        } else {
            sb.append('0');
            dump(x.left, sb);
            dump(x.right, sb);
        }
    }
    //+END_SOLUTION

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Node trie = trieFromText("abracadabra");
        //System.out.println(toDot(trie));
        Assert.assertEquals("01a01b01r01c1d", toDump(trie));
        System.out.println("OK");
    }
    //+END_FOLD }
}
