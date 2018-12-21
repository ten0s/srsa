public class Entry<K, V> {
    public K key;
    public V val;

    public Entry() {}
    public Entry(K key, V val) {
        this.key = key;
        this.val = val;
    }

    public String toString() {
        return "Entry{key=" + key + "," + "val=" + val + "}";
    }
}
