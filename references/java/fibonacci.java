public class fibonacci {
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
