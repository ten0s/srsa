import java.util.Objects;

public class Edge implements Comparable<Edge> {
    // SOLUTION_BEGIN
    private final int v;
    private final int w;
    private final double weight;
    // SOLUTION_END

    public Edge(int v, int w, double weight) {
        // SOLUTION_BEGIN
        this.v = v;
        this.w = w;
        this.weight = weight;
        // SOLUTION_END
    }

    public double weight() {
        // SOLUTION_BEGIN
        return weight;
        // SOLUTION_END
    }

    public int either() {
        // SOLUTION_BEGIN
        return v;
        // SOLUTION_END
    }

    public int other(int vertex) {
        // SOLUTION_BEGIN
        if      (vertex == v) return w;
        else if (vertex == w) return v;
        else throw new IllegalArgumentException();
        // SOLUTION_END
    }

    public int compareTo(Edge that) {
        // SOLUTION_BEGIN
        if      (this.weight < that.weight) return -1;
        else if (this.weight > that.weight) return +1;
        else                                return 0;
        // SOLUTION_END
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