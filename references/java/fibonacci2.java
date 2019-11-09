public class fibonacci2 {
  public static void main(String[] args) {
    System.out.println(fibonacci2.compute(46));
  }

  public static int compute(int i) {
    if (i == 0) {
      return 0;
    } else if (i == 1) {
      return 1;
    } else {
      return compute(i - 1) + compute(i - 2);
    }
  }
}
