public class Pair<A, B> {
    public A first;
    public B second;

    public Pair() {}
    public Pair(A first, B second) {
        this.first = first;
        this.second = second;
    }

    public String toString() {
        return "Pair{first=" + first + "," + "second=" + second + "}";
    }

    public boolean equals(Object x) {
        if (this == x) return true;
        if (x == null) return false;
        if (this.getClass() != x.getClass()) return false;
        Pair that = (Pair) x;
        return this.first.equals(that.first) &&
               this.second.equals(that.second);
    }
}
