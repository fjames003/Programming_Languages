import java.util.*;
   
   abstract class A {
     // added abstract keyword on m() for compile...
     abstract void m();
     void n() {
       System.out.println("A.n");
       this.m();
     }
   }

   class B extends A {
     void m() {
       System.out.println("B.m");
     }
     void n() {
       System.out.println("B.n");
     }
   }
   
   class C extends A {
     void m() {
       System.out.println("C.m");
     }
     void n(String msg) {
       System.out.println("C.n: " + msg);
     }
   }

   class Main {
     public static void main(String[] args) {
       List<A> as = new LinkedList<A>();
       as.add(new B());
       as.add(new C());
   
       for(A a : as)
         a.n();
     }
   }