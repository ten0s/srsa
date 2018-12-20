public class Entry<Key, Value> {
    public Key key;
    public Value value;

    public Entry() {}
    public Entry(Key key, Value value) {
        this.key = key;
        this.value = value;
    }
}
