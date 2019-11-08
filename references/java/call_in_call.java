public class call_in_call {
  public static void main(String[] args) {
    System.out.println("outer");
    inner();
  }

  public static void inner() {
    System.out.println("inner");
  }
}
