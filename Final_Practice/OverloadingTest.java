class A {
     void m(Object o) {
       System.out.println("Called A.m(Object)");
     }

     void m(Integer i) {
       System.out.println("Called A.m(Integer)");
     }
   }

   class Main {
     public static void main(String[] args) {
       A a = new A();
       a.m(5);
       a.m("hello");
       Object o = new Integer(5);
       a.m(o);
     }
   }