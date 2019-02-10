class ArrayReverse {
    public static int[] reverse(int[] a) {
        //+BEGIN_SOLUTION
        int n = a.length;
        for (int i = 0; i < n/2; i++) {
            int tmp = a[i];
            a[i] = a[n-i-1];
            a[n-i-1] = tmp;
        }
        return a;
        //+END_SOLUTION
    }

    //+BEGIN_FOLD Tests {
    public static void main(String[] args) throws Throwable {
        Assert.assertArrayEquals(new int[]{}, reverse(new int[] {}));
        Assert.assertArrayEquals(new int[]{1}, reverse(new int[] {1}));
        Assert.assertArrayEquals(new int[]{5,4,3,2,1}, reverse(new int[] {1,2,3,4,5}));
        Assert.assertArrayEquals(new int[]{6,5,4,3,2,1}, reverse(new int[] {1,2,3,4,5,6}));
        System.out.println("OK");
    }
    //+END_FOLD }
}
