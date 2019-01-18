import java.util.List;
import java.util.ArrayList;

public class WeightedQuickUnionPathCompressionUF {
    // SOLUTION_BEGIN
    private int[] parent;
    private int[] size;
    private int count;
    // SOLUTION_END

    public WeightedQuickUnionPathCompressionUF(int n) {
        // SOLUTION_BEGIN
        parent = new int[n];
        size = new int[n];
        for (int i = 0; i < n; i++) {
            parent[i] = i;
            size[i] = 1;
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
        if (p != parent[p]) {
            parent[p] = find(parent[p]);
            p = parent[p];
        }
        /*
        // Path halving (every other node on the path points to its grandparent)
        while (p != parent[p]) {
            parent[p] = parent[parent[p]];
            p = parent[p];
        }
        */
        return p;
        // SOLUTION_END
    }

    public void union(int p, int q) {
        // SOLUTION_BEGIN
        int rootP = find(p);
        int rootQ = find(q);
        if (rootP == rootQ) return;
        if (size[rootP] < size[rootQ]) {
            parent[rootP] = rootQ;
            size[rootQ] += size[rootP];
        } else {
            parent[rootQ] = rootP;
            size[rootP] += size[rootQ];
        }
        count--;
        // SOLUTION_END
    }

    // size of p's component
    public int size(int p) {
        // SOLUTION_BEGIN
        return size[find(p)];
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
            //System.out.println(p.first + " " + p.second + " " + ArrayUtil.toString(uf.parent));
            if (!uf.connected(p.first, p.second)) {
                uf.union(p.first, p.second);
            }
        }
        Assert.assertEquals(2, uf.count());

        Assert.assertEquals(6, uf.size(0));
        Assert.assertEquals(4, uf.size(9));

        Assert.assertTrue(uf.connected(0, 7));
        Assert.assertTrue(uf.connected(4, 9));
        Assert.assertFalse(uf.connected(0, 9));

        System.out.println("OK");
    }
}
