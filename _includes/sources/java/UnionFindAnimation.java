import java.util.Deque;
import java.util.ArrayDeque;
import java.io.FileWriter;
import java.io.IOException;

/*
#+BEGIN_SRC sh :results output drawer
make run CLASS=UnionFindAnimation
#+END_SRC

#+RESULTS:
:RESULTS:
:END:

 */

public class UnionFindAnimation {
    public static void main(String[] args) throws IOException {
        int n = 10;
        UnionFind uf = new UnionFind(n);
        Deque<Pair<Integer,Integer>> pairs = new ArrayDeque<>();
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

        Deque<String> cmds = new ArrayDeque<>();
        Deque<String> dots = new ArrayDeque<>();

        cmds.add("Init");
        dots.add(uf.toDot());

        for (Pair<Integer,Integer> p : pairs) {
            if (!uf.connected(p.first, p.second)) {
                uf.union(p.first, p.second);
            }
            cmds.add(String.format("Connect %d to %d", p.first, p.second));
            dots.add(uf.toDot());
        }

        cmds.add("Done");
        dots.add(uf.toDot());

        int i = 1;
        for (String cmd : cmds) {
            FileWriter file = new FileWriter(String.format("uf-%02d.cmd", i));
            file.write(cmd);
            file.close();
            i++;
        }
        i = 1;
        for (String dot : dots) {
            FileWriter file = new FileWriter(String.format("uf-%02d.dot", i));
            file.write(dot);
            file.close();
            i++;
        }
    }
}
