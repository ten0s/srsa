import java.util.Arrays;

class Main {
    // Reverse the elements within an array
    public static int[] reverse(int[] a) {
        int n = a.length;
        for (int i = 0; i < n/2; i++) {
            int tmp = a[i];
            a[i] = a[n-i-1];
            a[n-i-1] = tmp;
        }
        return a;
    }

    public static void main(String[] args) {
        try {
            assert Arrays.equals(new int[]{}, reverse(new int[] {}));
            assert Arrays.equals(new int[]{1}, reverse(new int[] {1}));
            assert Arrays.equals(new int[]{5,4,3,2,1}, reverse(new int[] {1,2,3,4,5}));
            assert Arrays.equals(new int[]{6,5,4,3,2,1}, reverse(new int[] {1,2,3,4,5,6}));
            System.out.println("OK");
        } catch (Throwable e) {
            System.out.println("FAIL");
            e.printStackTrace();
        }
    }
}
