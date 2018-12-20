public class BinTree {
    public static class Node<Key, Value> {
        public Key key;
        public Value value;
        public Node<Key, Value> left;
        public Node<Key, Value> right;
        public int size;

        public Node(Key key) {
            this.key = key;
            this.size = 1;
        }

        public Node(Key key, Value value) {
            this.key = key;
            this.value = value;
            this.size = 1;
        }
    }
}
