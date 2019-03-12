import java.util.*;

public class StringRegexp {
    private static class Regexp {
        //+BEGIN_SOLUTION
        private char[] re;
        private final int M;
        private Digraph G;
        //+END_SOLUTION

        public Regexp(String regexp) {
            //+BEGIN_SOLUTION
            re = regexp.toCharArray();
            M = re.length;
            G = epsilonTransitionDigraph();
            //+END_SOLUTION
        }

        //+BEGIN_SOLUTION
        private Digraph epsilonTransitionDigraph() {
            Digraph G = new Digraph(M+1);
            Deque<Integer> stack = new ArrayDeque<>();
            for (int i = 0; i < M; i++) {
                int lp = i;
                if (re[i] == '(' || re[i] == '|') {
                    stack.push(i);
                } else if (re[i] == ')') {
                    Deque<Integer> ors = new ArrayDeque<>();
                    while (true) {
                        int j = stack.pop();
                        if (re[j] == '|') {
                            ors.push(j);
                        } else {
                            lp = j;
                            break;
                        }
                    }
                    for (int or : ors) {
                        G.addEdge(lp, or+1);
                        G.addEdge(or, i);
                    }
                }
                if (i < M-1) {
                    if (re[i+1] == '*') {
                        G.addEdge(lp, i+1);
                        G.addEdge(i+1, lp);
                    }
                    if (re[i+1] == '+') {
                        G.addEdge(i+1, lp);
                    }
                }
                if (re[i] == '(' || re[i] == '*' || re[i] == '+' || re[i] == ')') {
                    G.addEdge(i, i+1);
                }
            }
            return G;
        }
        //+END_SOLUTION

        public boolean matches(String txt) {
            //+BEGIN_SOLUTION
            Deque<Integer> states = new ArrayDeque<>();
            DigraphDFS dfs = new DigraphDFS(G, 0);
            for (int v = 0; v < G.V(); v++)
                if (dfs.hasPathTo(v)) states.add(v);

            int N = txt.length();
            for (int i = 0; i < N; i++) {
                Deque<Integer> matches = new ArrayDeque<>();
                for (int s : states) {
                    if (s == M) continue;

                    if (re[s] == txt.charAt(i) || re[s] == '.')
                        matches.add(s+1);
                }

                states = new ArrayDeque<>();
                dfs = new DigraphDFS(G, matches);
                for (int v = 0; v < G.V(); v++)
                    if (dfs.hasPathTo(v)) states.add(v);
            }

            for (int s : states)
                if (s == M) return true;
            return false;
            //+END_SOLUTION
        }

        //+BEGIN_FOLD Utils {
        public String toDot() {
            StringBuilder sb = new StringBuilder();
            sb.append("digraph {");
            sb.append(System.lineSeparator());
            sb.append("  rankdir=LR");
            sb.append(System.lineSeparator());

            for (int v = 0; v < G.V(); v++) {
                char ch = v < re.length ? re[v] : ' ';
                sb.append("  " + v + " " + attrs(label(ch), xlabel(v), shape("circle")));
                sb.append(System.lineSeparator());
            }

            for (int v = 0; v < G.V()-1; v++) {
                if (!isMetaChar(re[v])) {
                    sb.append("  " + v + " -> " + (v+1) + " " + attrs(color("black")));
                    sb.append(System.lineSeparator());
                }
            }

            for (int v = 0; v < G.V(); v++) {
                for (int w : G.adj(v)) {
                    sb.append("  " + v + " -> " + w + " " + attrs(color("red")));
                    sb.append(System.lineSeparator());
                }
            }

            sb.append("}");
            sb.append(System.lineSeparator());
            return sb.toString();
        }

        private boolean isMetaChar(char ch) {
            switch (ch) {
            case '(':
            case ')':
            case '*':
            case '+':
            case '|':
                return true;
            default:
                return false;
            }
        }

        private String attrs(String... attrs) {
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < attrs.length; i++) {
                sb.append(attrs[i]);
                if (i != attrs.length-1) sb.append(", ");
            }
            sb.append("]");
            return sb.toString();
        }

        private String label(Object o) {
            return "label=" + Character.toString('"') + o + Character.toString('"');
        }

        private String xlabel(Object o) {
            return "xlabel=" + Character.toString('"') + o + Character.toString('"');
        }

        private String color(String c) {
            return "color=" + Character.toString('"') + c + Character.toString('"');
        }

        private String shape(String s) {
            return "shape=" + Character.toString('"') + s + Character.toString('"');
        }
        //+END_FOLD }
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Regexp re1 = new Regexp("(A|B)(C|D)");
        //System.out.println(re1.toDot());
        Assert.assertTrue(re1.matches("AC"));
        Assert.assertTrue(re1.matches("AD"));
        Assert.assertTrue(re1.matches("BC"));
        Assert.assertTrue(re1.matches("BD"));
        Assert.assertFalse(re1.matches("AB"));
        Assert.assertFalse(re1.matches("CD"));

        Regexp re2 = new Regexp("(A(B|C)*D)");
        //System.out.println(re2.toDot());
        Assert.assertTrue(re2.matches("AD"));
        Assert.assertTrue(re2.matches("ABD"));
        Assert.assertTrue(re2.matches("ACD"));
        Assert.assertTrue(re2.matches("ABCCBD"));
        Assert.assertFalse(re2.matches("BCD"));
        Assert.assertFalse(re2.matches("ADD"));
        Assert.assertFalse(re2.matches("ABCBC"));

        Regexp re3 = new Regexp("(A*|(A*BA**BA*)*)");
        //System.out.println(re3.toDot());
        Assert.assertTrue(re3.matches("AAA"));
        Assert.assertTrue(re3.matches("BBAABB"));
        Assert.assertTrue(re3.matches("BABAAA"));
        Assert.assertFalse(re3.matches("ABA"));
        Assert.assertFalse(re3.matches("BBB"));
        Assert.assertFalse(re3.matches("BABBAAA"));

        Regexp re4 = new Regexp("((AB)+)");
        //System.out.println(re4.toDot());
        Assert.assertTrue(re4.matches("AB"));
        Assert.assertTrue(re4.matches("ABABAB"));
        Assert.assertFalse(re4.matches(""));
        Assert.assertFalse(re4.matches("BBBAAA"));

        Regexp re5 = new Regexp("(A(B|C|D)E)");
        //System.out.println(re5.toDot());
        Assert.assertTrue(re5.matches("ABE"));
        Assert.assertTrue(re5.matches("ACE"));
        Assert.assertTrue(re5.matches("ADE"));
        Assert.assertFalse(re5.matches("AAE"));
        Assert.assertFalse(re5.matches("AEE"));

        Regexp re6 = new Regexp("A.E");
        //System.out.println(re6.toDot());
        Assert.assertTrue(re6.matches("AAE"));
        Assert.assertTrue(re6.matches("ABE"));
        Assert.assertTrue(re6.matches("ACE"));
        Assert.assertTrue(re6.matches("ADE"));
        Assert.assertTrue(re6.matches("AEE"));
        Assert.assertTrue(re6.matches("AZE"));

        System.out.println("OK");
    }
    //+END_FOLD }
}

// Refs
/*+BEGIN_FOLD
public class Digraph {
    public Digraph(int V);
    public int V();
    public void addEdge(int v, int w);
}

public class DigraphDFS {
    public DigraphDFS(Digraph G, int s);
    public DigraphDFS(Digraph G, Iterable<Integer> sources);
    public boolean hasPathTo(int v);
}
+END_FOLD*/
