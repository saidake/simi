package com.simi.common.util.encode;

import org.apache.commons.codec.binary.Base64;
import org.bouncycastle.jce.provider.BouncyCastleProvider;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.net.URLDecoder;
import java.security.*;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

public class RSAUtil {

    private static final BouncyCastleProvider PROVIDER = new BouncyCastleProvider();

    /**
     * Default signing algorithm "SHA1WithRSA"
     * The "SHA1WithRSA" algorithm uses the same private key to sign the same text, and the signature remains the same every time.
     */
    public static final String SIGN_ALGORITHMS = "SHA1WithRSA";

    /**
     * "SHA256WithRSA" signing algorithm, which uses the same private key to sign the same text, and the signature remains the same every time.
     */
    public static final String SIGN_ALGORITHMS_SHA256 = "SHA256WithRSA";

    /**
     * Signs the provided text using a private key.
     *
     * @param signText The text to be signed.
     * @param keyFileName The filename of the private key.
     * @param keyPassword The password for the private key.
     * @return A Base64-encoded signature.
     * @throws Exception If the signing process fails, an exception will be thrown.
     */
    public String sign(String signText, String keyFileName, String keyPassword) throws Exception {
        try {
            byte[] keyBytes = loadKey(keyFileName, keyPassword); // Load the private key
            PKCS8EncodedKeySpec pkcs8KeySpec = new PKCS8EncodedKeySpec(keyBytes);
            KeyFactory keyFactory = KeyFactory.getInstance("RSA");
            Signature signature = Signature.getInstance("SHA256withRSA");
            signature.initSign(keyFactory.generatePrivate(pkcs8KeySpec)); // Initialize the signature with the private key
            signature.update(signText.getBytes("UTF-8")); // Update the signature with the text
            byte[] result = signature.sign(); // Sign the text
            return Base64.encodeBase64String(result); // Return the Base64-encoded signature
        } catch (Exception ex) {
            throw new Exception("Encryption failed", ex); // Throw exception if signing fails
        }
    }

    /**
     * Loads the key from the specified file.
     *
     * @param keyFileName The filename of the key.
     * @param keyPassword The password used for decryption, if applicable.
     * @return The decoded key as a byte array.
     * @throws NoSuchAlgorithmException If the algorithm is not found.
     * @throws Exception If there is an issue reading or decrypting the key.
     */
    private byte[] loadKey(String keyFileName, String keyPassword) throws NoSuchAlgorithmException, Exception {
        String signKey;
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(URLDecoder.decode(keyFileName, "UTF-8"))); // Read the key file
            StringBuilder sb = new StringBuilder();
            String readLine;
            while ((readLine = reader.readLine()) != null) {
                sb.append(readLine); // Append each line of the key
            }

            if (keyPassword != null) {
                Key desKey = DESUtil.generateKey(keyPassword); // Generate DES key from password
                signKey = DESUtil.decryptDES(sb.toString(), desKey); // Decrypt the key if a password is provided
            } else {
                signKey = sb.toString(); // Use the key directly if no password is provided
            }
        } finally {
            if (reader != null) {
                reader.close(); // Close the reader
            }
        }
        return Base64.decodeBase64(signKey); // Return the decoded key
    }

    /**
     * Verifies the signature of the provided text using a public key.
     *
     * @param verifiText The text to be verified.
     * @param signedText The Base64-encoded signature.
     * @param keyFileName The filename of the public key.
     * @param keyPassword The password for the public key.
     * @throws Exception If the verification fails.
     */
    public void verify(String verifiText, String signedText, String keyFileName, String keyPassword) throws Exception {
        boolean verifyResult;
        try {
            byte[] keyBytes = loadKey(keyFileName, keyPassword); // Load the public key
            X509EncodedKeySpec keySpec = new X509EncodedKeySpec(keyBytes);
            KeyFactory keyFactory = KeyFactory.getInstance("RSA");
            Signature signature = Signature.getInstance("SHA256withRSA");
            signature.initVerify(keyFactory.generatePublic(keySpec)); // Initialize the signature with the public key
            signature.update(verifiText.getBytes("UTF-8")); // Update the signature with the text
            byte[] verifyBytes = Base64.decodeBase64(signedText); // Decode the signed text
            verifyResult = signature.verify(verifyBytes); // Verify the signature
        } catch (Exception ex) {
            throw new Exception("Verification failed for parameters [" + verifiText + " and " + signedText + "]", ex);
        }
        if (!verifyResult) {
            throw new Exception("Verification failed"); // Throw exception if verification fails
        }
    }

    /**
     * Returns a PublicKey object from a Base64-encoded key string.
     *
     * @param key The Base64-encoded public key string.
     * @return The PublicKey object.
     * @throws Exception If an error occurs during key parsing.
     */
    public static PublicKey getPublicKey(String key) throws Exception {
        byte[] keyBytes = Base64.decodeBase64(key); // Decode the Base64-encoded key
        X509EncodedKeySpec keySpec = new X509EncodedKeySpec(keyBytes);
        KeyFactory keyFactory = KeyFactory.getInstance("RSA");
        return keyFactory.generatePublic(keySpec); // Generate and return the PublicKey object
    }

    /**
     * Returns a PrivateKey object from a Base64-encoded key string.
     *
     * @param key The Base64-encoded private key string.
     * @return The PrivateKey object.
     * @throws Exception If an error occurs during key parsing.
     */
    public static PrivateKey getPrivateKey(String key) throws Exception {
        byte[] keyBytes = Base64.decodeBase64(key); // Decode the Base64-encoded key
        PKCS8EncodedKeySpec keySpec = new PKCS8EncodedKeySpec(keyBytes);
        KeyFactory keyFactory = KeyFactory.getInstance("RSA");
        return keyFactory.generatePrivate(keySpec); // Generate and return the PrivateKey object
    }

    /**
     * Verifies the RSA signature using a specific signing algorithm and public key.
     *
     * @param content The content to be verified.
     * @param sign The Base64-encoded signature.
     * @param publicKey The public key to verify the signature.
     * @param signAlgorithm The signing algorithm to use.
     * @param charsetName The character set for encoding the content.
     * @return true if the signature is valid, false otherwise.
     * @throws Exception If an error occurs during verification.
     */
    public static boolean checkSign(String content, String sign, PublicKey publicKey, String signAlgorithm, String charsetName) throws Exception {
        Signature signature = Signature.getInstance(signAlgorithm, PROVIDER); // Create signature instance
        signature.initVerify(publicKey); // Initialize the signature with the public key
        signature.update(content.getBytes(charsetName)); // Update the signature with the content
        return signature.verify(decodeBASE64(sign)); // Verify the signature
    }

    /**
     * Verifies the RSA signature using the default signing algorithm (SHA1WithRSA) and public key.
     * Uses UTF-8 encoding by default.
     *
     * @param content The content to be verified.
     * @param sign The Base64-encoded signature.
     * @param publicKey The public key to verify the signature.
     * @return true if the signature is valid, false otherwise.
     * @throws Exception If an error occurs during verification.
     */
    public static boolean checkSign(String content, String sign, PublicKey publicKey) throws Exception {
        return checkSign(content, sign, publicKey, SIGN_ALGORITHMS, "utf-8"); // Use default algorithm and charset
    }

    /**
     * Verifies the RSA signature using the SHA256WithRSA algorithm and public key.
     * Uses UTF-8 encoding by default.
     *
     * @param content The content to be verified.
     * @param sign The Base64-encoded signature.
     * @param publicKey The public key to verify the signature.
     * @return true if the signature is valid, false otherwise.
     * @throws Exception If an error occurs during verification.
     */
    public static boolean shaCheckSign(String content, String sign, PublicKey publicKey) throws Exception {
        return checkSign(content, sign, publicKey, SIGN_ALGORITHMS_SHA256, "utf-8"); // Use SHA256 algorithm
    }

    /**
     * Encodes a byte array to a Base64 string.
     *
     * @param bytes The byte array to encode.
     * @return The Base64-encoded string.
     * @throws Exception If an error occurs during encoding.
     */
    public static String encodeBASE64(byte[] bytes) throws Exception {
        return Base64.encodeBase64String(bytes); // Return the Base64-encoded string
    }

    /**
     * Decodes a Base64-encoded string to a byte array.
     *
     * @param base64 The Base64-encoded string to decode.
     * @return The decoded byte array.
     * @throws Exception If an error occurs during decoding.
     */
    public static byte[] decodeBASE64(String base64) throws Exception {
        return Base64.decodeBase64(base64); // Return the decoded byte array
    }
}
