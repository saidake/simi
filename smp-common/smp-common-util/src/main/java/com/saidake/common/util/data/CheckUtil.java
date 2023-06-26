package com.saidake.common.util.data;

public class CheckUtil {
    public static void throwErrorMessage(String message) {
        throw new RuntimeException(message);
    }

    public static void checkFalse(Boolean flag, String message) {
        if (!flag) {
            throw new RuntimeException(message);
        }
    }

    public static void checkTrue(Boolean flag, String message) {
        if (flag) {
            throw new RuntimeException(message);
        }
    }

    public static void checkNull(Object flag, String errorMessage) {
        if (flag == null) {
            throw new RuntimeException(errorMessage);
        }
    }

    public static void checkNotNull(Object flag, String message) {
        if (flag != null) {
            throw new RuntimeException(message);
        }
    }
}
