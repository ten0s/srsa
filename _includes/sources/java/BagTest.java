import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertArrayEquals;
import org.junit.*;
import java.util.Arrays;
import java.util.NoSuchElementException;

// $ make test CLASS=Bag

public class BagTest {
    @Test
    public void defaultConstructor() {
        Bag<String> b = new Bag<>();
        assertTrue(b.isEmpty());
        assertEquals(0, b.size());
    }

    @Test
    public void checkNotEmpty() {
        Bag<String> b = new Bag<>();
        b.add("1");
        b.add("2");
        b.add("3");
        assertFalse(b.isEmpty());
        assertEquals(3, b.size());
    }

    @Test
    public void removeEmpty() {
        Bag<String> b = new Bag<>();
        b.remove("1");
        assertArrayEquals(new String[] {},
                          b.toArray(new String[b.size()]));
    }

    @Test
    public void removeNotExisting() {
        Bag<String> b = new Bag<>();
        b.add("1");
        b.add("2");
        b.add("3");
        b.remove("4");
        assertArrayEquals(new String[] {"3", "2", "1"},
                          b.toArray(new String[b.size()]));
    }

    @Test
    public void removeFirst() {
        Bag<String> b = new Bag<>();
        b.add("1");
        b.add("2");
        b.add("3");
        b.remove("3");
        assertArrayEquals(new String[] {"2", "1"},
                          b.toArray(new String[b.size()]));
    }

    @Test
    public void removeMiddle() {
        Bag<String> b = new Bag<>();
        b.add("1");
        b.add("2");
        b.add("3");
        b.remove("2");
        assertArrayEquals(new String[] {"3", "1"},
                          b.toArray(new String[b.size()]));
    }

    @Test
    public void removeLast() {
        Bag<String> b = new Bag<>();
        b.add("1");
        b.add("2");
        b.add("3");
        b.remove("1");
        assertArrayEquals(new String[] {"3", "2"},
                          b.toArray(new String[b.size()]));
    }

    @Test
    public void removeSome() {
        Bag<String> b = new Bag<>();
        b.add("1");
        b.add("2");
        b.add("3");
        b.add("1");
        b.remove("1");
        assertArrayEquals(new String[] {"3", "2", "1"},
                          b.toArray(new String[b.size()]));
    }

    @Test
    public void isMemberEmpty() {
        Bag<String> b = new Bag<>();
        assertFalse(b.isMember("1"));
    }

    @Test
    public void isMemberNotExisting() {
        Bag<String> b = new Bag<>();
        b.add("1");
        b.add("2");
        b.add("3");
        assertFalse(b.isMember("4"));
    }

    @Test
    public void isMemberFirst() {
        Bag<String> b = new Bag<>();
        b.add("1");
        b.add("2");
        b.add("3");
        assertTrue(b.isMember("1"));
    }

    @Test
    public void isMemberMiddle() {
        Bag<String> b = new Bag<>();
        b.add("1");
        b.add("2");
        b.add("3");
        assertTrue(b.isMember("2"));
    }

    @Test
    public void isMemberLast() {
        Bag<String> b = new Bag<>();
        b.add("1");
        b.add("2");
        b.add("3");
        assertTrue(b.isMember("3"));
    }

    @Test
    public void iteratorEmpty() {
        Bag<String> b = new Bag<>();
        for (String j : b) {}
    }

    @Test
    public void iteratorNotEmpty() {
        Bag<String> b = new Bag<>();
        b.add("1");
        b.add("2");
        b.add("3");
        String[] r = new String[b.size()];
        int i = 0;
        for (String j : b) {
            r[i++] = j;
        }
        assertArrayEquals(new String[] {"3", "2", "1"}, r);
    }

    @Test
    public void toArrayEmpty() {
        Bag<String> b = new Bag<>();
        String[] a = b.toArray(new String[b.size()]);
        assertArrayEquals(new String[] {}, a);
    }

    @Test
    public void toArrayNotEmpty() {
        Bag<String> b = new Bag<>();
        b.add("1");
        b.add("2");
        b.add("3");
        String[] a = b.toArray(new String[b.size()]);
        assertArrayEquals(new String[] {"3", "2", "1"}, a);
    }
}
