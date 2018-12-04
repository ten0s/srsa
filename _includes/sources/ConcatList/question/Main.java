import java.util.Arrays;

public class Main {
    static class Node {
        int item;
        Node next;
    }

    public static Node concat(Node n1, Node n2) {

    }

    public static void main(String[] args) {
        try {
            // [1,2,3]
            Node n1 = new Node();
            n1.item = 1;
            Node n2 = new Node();
            n2.item = 2;
            n1.next = n2;
            Node n3 = new Node();
            n3.item = 3;
            n2.next = n3;

            // [4,5,6]
            Node n4 = new Node();
            n4.item = 4;
            Node n5 = new Node();
            n5.item = 5;
            n4.next = n5;
            Node n6 = new Node();
            n6.item = 6;
            n5.next = n6;

            assertArrayEquals(new int[] {}, toArray(concat(null, null)));
            assertArrayEquals(new int[] {1,2,3}, toArray(concat(n1, null)));
            assertArrayEquals(new int[] {4,5,6}, toArray(concat(null, n4)));
            assertArrayEquals(new int[] {1,2,3,4,5,6}, toArray(concat(n1, n4)));

            System.out.println("OK");
        } catch (Throwable e) {
            System.out.println("FAIL");
            e.printStackTrace();
        }
    }

    private static void assertArrayEquals(int[] exp, int[] act) throws Throwable {
        if (!Arrays.equals(exp, act)) {
            System.out.println("Expected: " + arrayToString(exp) +
                               ", but saw: " + arrayToString(act));
            throw new Throwable();
        }
    }

    private static int[] toArray(Node node) {
        int size = 0;
        for (Node n = node; n != null; n = n.next) {
            size++;
        }
        int[] a = new int[size];
        int i = 0;
        for (Node n = node; n != null; n = n.next) {
			a[i++] = n.item;
		}
        return a;
    }

    private static String arrayToString(int[] a) {
        String s = "[";
        for (int i = 0; i < a.length; i++) {
            s += a[i];
            if (i != a.length-1) {
                s += ",";
            }
        }
        s += "]";
        return s;
    }
}
