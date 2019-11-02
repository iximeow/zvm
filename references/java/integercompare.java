public class integercompare {
  public static void main(String args[]) {
    Integer constructed = new Integer(1);
    Integer valueOfed = Integer.valueOf(1);
    System.out.println(constructed == valueOfed);
    System.out.println((Object)constructed == (Object)valueOfed);
    System.out.println((constructed + 1) == (valueOfed + 1));
  }
}
