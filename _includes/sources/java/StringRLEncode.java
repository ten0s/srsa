public class StringRLEncode extends StringRLE {
    public static String encode(String s) {
        //+BEGIN_SOLUTION
        final int n = s.length();
        if (n == 0) return "";
        StringBuilder sb = new StringBuilder();
        char symbol = s.charAt(0);
        int count = 1;
        for (int i = 1; i < n; i++) {
            if (s.charAt(i) != symbol) {
                sb.append(symbol).append(count);
                symbol = s.charAt(i);
                count = 1;
            } else {
                count++;
            }
        }
        sb.append(symbol).append(count);
        return sb.toString();
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Assert.assertEquals("", decode(encode("")));
        Assert.assertEquals("", encode(decode("")));
        Assert.assertEquals("A", decode(encode("A")));
        Assert.assertEquals("A1", encode(decode("A1")));
        Assert.assertEquals("AAAAAAAAAA", decode(encode("AAAAAAAAAA")));
        Assert.assertEquals("A10", encode(decode("A10")));
        Assert.assertEquals("AAABBC", decode(encode("AAABBC")));
        Assert.assertEquals("A3B2C1", encode(decode("A3B2C1")));
        System.out.println("OK");
    }
    //+END_FOLD }
}
