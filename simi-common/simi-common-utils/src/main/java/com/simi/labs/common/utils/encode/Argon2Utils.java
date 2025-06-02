package com.simi.labs.common.utils.encode;

import de.mkammerer.argon2.Argon2;
import de.mkammerer.argon2.Argon2Factory;

public class Argon2Utils {

    // Configuration parameters, can be adjusted for production environment
    private static final int SALT_LENGTH = 16;        // Salt length (not directly used but useful reference)
    private static final int HASH_LENGTH = 32;        // Length of generated hash (not directly used here)
    private static final int ITERATIONS = 3;          // Number of iterations
    private static final int MEMORY = 65536;           // Memory usage in KB
    private static final int PARALLELISM = 1;          // Degree of parallelism

    private static final Argon2 argon2 = Argon2Factory.create();

    /**
     * Hashes the given password and returns the hash string
     * which contains salt and parameter info.
     */
    public static String hashPassword(char[] password) {
        try {
            return argon2.hash(ITERATIONS, MEMORY, PARALLELISM, password);
        } finally {
            argon2.wipeArray(password); // Clear password from memory for security
        }
    }

    /**
     * Verifies if the given password matches the stored hash.
     */
    public static boolean verifyPassword(String hash, char[] password) {
        try {
            return argon2.verify(hash, password);
        } finally {
            argon2.wipeArray(password); // Clear password from memory for security
        }
    }

    // Simple test method
    public static void main(String[] args) {
        String rawPassword = "myStrongPassword123!";
        String hash = hashPassword(rawPassword.toCharArray());
        System.out.println("Hash: " + hash);

        boolean match = verifyPassword(hash, rawPassword.toCharArray());
        System.out.println("Password match? " + match);
    }
}
