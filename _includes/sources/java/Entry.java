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

    public boolean equals(Object x) {
        if (this == x) return true;
        if (x == null) return false;
        if (this.getClass() != x.getClass()) return false;
        Entry that = (Entry) x;
        return this.key.equals(that.key) &&
               this.val.equals(that.val);
    }
}
