import java.util.Arrays;

public class Main {
    static class Node {
        int item;
        Node next;
    }

    public static Node delete(int key, Node node) {

    }

    public static void main(String[] args) {
        try {
            Node n = null;
            n = delete(1, n);
            assert Arrays.equals(new int[] {}, toArray(n));

            n = new Node();
            n.item = 1;

            Node n2 = new Node();
            n2.item = 2;
            n.next = n2;

            Node n3 = new Node();
            n3.item = 3;
            n2.next = n3;

            Node n4 = new Node();
            n4.item = 4;
            n3.next = n4;

            Node n5 = new Node();
            n5.item = 5;
            n4.next = n5;

            assertArrayEquals(new int[] {1,2,3,4,5}, toArray(n));

            n = delete(1, n);
            assertArrayEquals(new int[] {2,3,4,5}, toArray(n));

            n = delete(3, n);
            assertArrayEquals(new int[] {2,4,5}, toArray(n));

            n = delete(5, n);
            assertArrayEquals(new int[] {2,4}, toArray(n));

            n = delete(6, n);
            assertArrayEquals(new int[] {2,4}, toArray(n));

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
