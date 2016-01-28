class A {
     void m(Object o) {
       System.out.println("A.m(Object)");
     }
   }

   class B extends A {
     void m(Integer i) {
       System.out.println("B.m(Integer)");
     }
   }

   class Main {
     public static void main(String[] args) {
       A a = new A();
       B b = new B();

       Object o = new Integer(5);

       a.m(5);
       a.m("hello");
       a.m(o);

       b.m(5);
       b.m("hello");
       b.m(o);
     }
   }