public class exception {
  public static void main(String args[]) {
    System.out.println("do exceptions work?");
    try {
      if (args.length > 1) {
        throw new RuntimeException("hello");
      } else {
        System.out.println("no exception");
      }
    } catch (Exception e) {
      System.out.println("caught an exception! let's take a look...");
      System.out.println(e);
    }
  }
}
