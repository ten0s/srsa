import java.util.List;
import java.util.ArrayList;

public class WeightedQuickUnionPathCompressionUF {
    // SOLUTION_BEGIN
    private int[] id;
    private int[] sz;
    private int count;
    // SOLUTION_END

    public WeightedQuickUnionPathCompressionUF(int n) {
        // SOLUTION_BEGIN
        id = new int[n];
        sz = new int[n];
        for (int i = 0; i < n; i++) {
            id[i] = i;
            sz[i] = 1;
        }
        count = n;
        // SOLUTION_END
    }

    public int count() {
        // SOLUTION_BEGIN
        return count;
        // SOLUTION_END
    }

    public boolean connected(int p, int q) {
        // SOLUTION_BEGIN
        return find(p) == find(q);
        // SOLUTION_END
    }

    public int find(int p) {
        // SOLUTION_BEGIN
        // https://en.wikipedia.org/wiki/Disjoint-set_data_structure#Path_compression
        // Path compression (every node on the path points to the root)
        if (p != id[p]) {
            id[p] = find(id[p]);
            p = id[p];
        }
        /*
        // Path halving (every other node on the path points to its grandparent)
        while (p != id[p]) {
            id[p] = id[id[p]];
            p = id[p];
        }
        */
        return p;
        // SOLUTION_END
    }

    public void union(int p, int q) {
        // SOLUTION_BEGIN
        int i = find(p);
        int j = find(q);
        if (i == j) return;
        if (sz[i] < sz[j]) { id[i] = j; sz[j] += sz[i]; }
        else               { id[j] = i; sz[i] += sz[j]; }
        count--;
        // SOLUTION_END
    }

    public static void main(String[] args) throws Throwable {
        int n = 10;
        WeightedQuickUnionPathCompressionUF uf = new WeightedQuickUnionPathCompressionUF(n);
        List<Pair<Integer,Integer>> pairs = new ArrayList<>();
        pairs.add(new Pair<Integer,Integer>(4,3));
        pairs.add(new Pair<Integer,Integer>(3,8));
        pairs.add(new Pair<Integer,Integer>(6,5));
        pairs.add(new Pair<Integer,Integer>(9,4));
        pairs.add(new Pair<Integer,Integer>(2,1));
        pairs.add(new Pair<Integer,Integer>(8,9));
        pairs.add(new Pair<Integer,Integer>(5,0));
        pairs.add(new Pair<Integer,Integer>(7,2));
        pairs.add(new Pair<Integer,Integer>(6,1));
        pairs.add(new Pair<Integer,Integer>(1,0));
        pairs.add(new Pair<Integer,Integer>(6,7));
        for (Pair<Integer,Integer> p : pairs) {
            //System.out.println(p.first + " " + p.second + " " + ArrayUtil.toString(uf.id));
            if (!uf.connected(p.first, p.second)) {
                uf.union(p.first, p.second);
            }
        }
        Assert.assertEquals(2, uf.count());
        Assert.assertTrue(uf.connected(0, 7));
        Assert.assertTrue(uf.connected(4, 9));
        Assert.assertFalse(uf.connected(0, 9));
        System.out.println("OK");
    }
}
