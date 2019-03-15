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

public class HuffmanTrieFromText extends Huffman {
    public static Node trieFromText(String text) {
        //+BEGIN_SOLUTION
        int[] freq = new int[R];
        int n = text.length();
        for (int i = 0; i < n; i++) {
            freq[text.charAt(i)]++;
        }
        PriorityQueue<Node> pq = new PriorityQueue<>();
        for (char c = 0; c < R; c++) {
            if (freq[c] > 0) {
                pq.add(new Node(c, freq[c], null, null));
            }
        }
        while (pq.size() > 1) {
            Node l = pq.remove();
            Node r = pq.remove();
            pq.add(new Node('\0', l.freq + r.freq, l, r));
        }
        return pq.remove();
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Node trie = trieFromText("abracadabra");
        //System.out.println(toDot(trie));
        Assert.assertEquals("01a01b01r01c1d", toDump(trie));
        System.out.println("OK");
    }
    //+END_FOLD }
}
