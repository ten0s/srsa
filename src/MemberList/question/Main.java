public class Main {
    static class Node {
        int item;
        Node next;
    }

    public static boolean isMember(int key, Node node) {

    }

    public static void main(String[] args) {
        try {
            Node n = null;
            assert !isMember(1, n);

            n = new Node();
            n.item = 1;

            Node n2 = new Node();
            n2.item = 2;
            n.next = n2;

            Node n3 = new Node();
            n3.item = 3;
            n2.next = n3;

            assert isMember(1, n);
            assert isMember(2, n);
            assert isMember(3, n);
            assert !isMember(4, n);

            System.out.println("OK");
        } catch (Throwable e) {
            System.out.println("FAIL");
            e.printStackTrace();
        }
    }
}
