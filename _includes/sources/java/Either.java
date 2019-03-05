public class Either {
    private Object[] vals;

    public Either(Object... vals) {
        this.vals = vals;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (int i = 0; i < vals.length; i++) {
            sb.append(vals[i]);
            if (i != vals.length-1) sb.append(", ");
        }
        sb.append("]");
        return sb.toString();
    }

    public boolean equals(Object val) {
        for (int i = 0; i < vals.length; i++) {
            if (vals[i].equals(val)) return true;
        }
        return false;
    }
}
