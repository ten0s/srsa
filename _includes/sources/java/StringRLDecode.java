public class StringRLDecode extends StringRLE {
    public static String decode(String s) {
        //+BEGIN_SOLUTION
        final int n = s.length();
        if (n == 0) return "";
        StringBuilder sb = new StringBuilder();
        int i = 0;
        while (i < n) {
            char symbol = s.charAt(i++);
            StringBuilder digits = new StringBuilder();
            while (i < n) {
                char ch = s.charAt(i);
                if (Character.isDigit(ch)) {
                    digits.append(ch);
                    i++;
                } else {
                    break;
                }
            }
            int count = Integer.parseInt(digits.toString());
            for (int k = 0; k < count; k++) {
                sb.append(symbol);
            }
        }
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
