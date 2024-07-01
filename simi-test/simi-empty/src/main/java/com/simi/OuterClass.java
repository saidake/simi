package com.simi;

public class OuterClass {

    private String message = "Outer: Hello, World!";
    private static String static_message = "Outer static: Hello, World!";

    void display() {
        String message = "Hello, World!";

        class LocalInnerClass {
            void printMessage() {
                System.out.println(message);
                System.out.println(OuterClass.this.message);
                System.out.println(static_message);

            }
        }

        LocalInnerClass local = new LocalInnerClass();
        local.printMessage();
    }
    static void displayStatic() {
        String message = "Hello, World!";

        class LocalInnerClass {
            void printMessage() {
                System.out.println(message);
                System.out.println(static_message);

            }
        }

        LocalInnerClass local = new LocalInnerClass();
        local.printMessage();
    }
    public static void main(String[] args) {
        OuterClass outer = new OuterClass();
        outer.display();
    }
}
