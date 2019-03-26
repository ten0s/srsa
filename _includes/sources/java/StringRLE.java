public class StringRLE {
    public static String encode(String s) {
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
    }

    public static String decode(String s) {
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
            for (int j = 0; j < count; j++) {
                sb.append(symbol);
            }
        }
        return sb.toString();
    }
}
