import java.util.Objects;

public class Edge implements Comparable<Edge> {
    // BEGIN_SOLUTION
    private final int v;
    private final int w;
    private final double weight;
    // END_SOLUTION

    public Edge(int v, int w, double weight) {
        // BEGIN_SOLUTION
        this.v = v;
        this.w = w;
        this.weight = weight;
        // END_SOLUTION
    }

    public double weight() {
        // BEGIN_SOLUTION
        return weight;
        // END_SOLUTION
    }

    public int either() {
        // BEGIN_SOLUTION
        return v;
        // END_SOLUTION
    }

    public int other(int vertex) {
        // BEGIN_SOLUTION
        if      (vertex == v) return w;
        else if (vertex == w) return v;
        else throw new IllegalArgumentException();
        // END_SOLUTION
    }

    public int compareTo(Edge that) {
        // BEGIN_SOLUTION
        if      (this.weight < that.weight) return -1;
        else if (this.weight > that.weight) return +1;
        else                                return 0;
        // END_SOLUTION
    }

    public String toString() {
        return String.format("%d-%d %.5f", v, w, weight);
    }

    public boolean equals(Object x) {
        if (x == this) return true;
        if (x == null) return false;
        if (x.getClass() != this.getClass()) return false;
        Edge that = (Edge) x;
        return this.v == that.v &&
               this.w == this.w &&
               this.weight == that.weight;
    }

    public int hashCode() {
        return Objects.hash(v, w, weight);
    }

    public static void main(String[] args) {

    }
}
