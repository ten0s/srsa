import java.util.function.Predicate;
import java.util.function.Function;
import java.util.function.BiFunction;

/*
public class Node<T> {
    T item;
    Node<T> next;
    public static <T> Node<T> empty();
    public static <T> Node<T> cons(T value, Node<T> list);
    public static <T> T head(Node<T> list);
    public static <T> Node<T> tail(Node<T> list);
    public static <T> boolean isEmpty(Node<T> list);
}
*/

public class ListFilterMapFold {
    public static <T> Node<T> filter(Predicate<T> pred, Node<T> list) {
        //+BEGIN_SOLUTION
        if (Node.isEmpty(list)) {
            return list;
        } else {
            T head = Node.head(list);
            Node<T> tail = Node.tail(list);
            if (pred.test(head)) {
                return Node.cons(head, filter(pred, tail));
            } else {
                return filter(pred, tail);
            }
        }
        //+END_SOLUTION
    }

    public static <T, R> Node<R> map(Function<T, R> fun, Node<T> list) {
        //+BEGIN_SOLUTION
        if (Node.isEmpty(list)) {
            return Node.empty();
        } else {
            return Node.cons(fun.apply(Node.head(list)), map(fun, Node.tail(list)));
        }
        //+END_SOLUTION
    }

    public static <T, R> R foldl(BiFunction<T, R, R> fun, R init, Node<T> list) {
        //+BEGIN_SOLUTION
        if (Node.isEmpty(list)) {
            return init;
        } else {
            return foldl(fun,
                         fun.apply(Node.head(list), init),
                         Node.tail(list));
        }
        //+END_SOLUTION
    }

    public static <T, R> R foldr(BiFunction<T, R, R> fun, R init, Node<T> list) {
        //+BEGIN_SOLUTION
        if (Node.isEmpty(list)) {
            return init;
        } else {
            return fun.apply(Node.head(list),
                             foldr(fun, init, Node.tail(list)));
        }
        //+END_SOLUTION
    }

    public static void main(String[] args) throws Throwable {
        Node<Integer> intList = Node.fromIntArray(new int[] {1,2,3,4,5});
        // filter
        Assert.assertArrayEquals(new int[] {2,4},
                                 Node.toIntArray(filter(i -> i % 2 == 0, intList)));
        // map
        Assert.assertArrayEquals(new int[] {2,4,6,8,10},
                                 Node.toIntArray(map(i -> 2 * i, intList)));
        // fold{l,r}
        Assert.assertEquals(Node.length(intList), foldl((i, acc) -> 1 + acc, 0, intList));
        Assert.assertEquals(Node.length(intList), foldr((i, acc) -> 1 + acc, 0, intList));

        Assert.assertEquals(Node.max(intList), foldl(Math::max, Integer.MIN_VALUE, intList));
        Assert.assertEquals(Node.max(intList), foldr(Math::max, Integer.MIN_VALUE, intList));

        Assert.assertArrayEquals(new int[] {5,4,3,2,1},
                                 Node.toIntArray(foldl(Node::cons, Node.empty(), intList)));
        Assert.assertArrayEquals(new int[] {1,2,3,4,5},
                                 Node.toIntArray(foldr(Node::cons, Node.empty(), intList)));
        System.out.println("OK");
    }
}
