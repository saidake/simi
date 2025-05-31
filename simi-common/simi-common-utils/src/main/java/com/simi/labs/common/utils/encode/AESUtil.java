package com.simi.labs.common.utils.encode;

import lombok.experimental.UtilityClass;

import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

@UtilityClass
public class AESUtil {
    /**
     * Encrypts a string using AES encryption with GCM mode.
     * @param srcString The plaintext string to encrypt.
     * @param encryptKey The key used for encryption.
     * @param iv The initialization vector used for encryption.
     * @return The encrypted string in hexadecimal format.
     * @throws Exception If encryption fails.
     */
    public static String encryptAES(String srcString, String encryptKey, String iv) throws Exception {
        try {
            // Create a key specification for the encryption key
            SecretKeySpec keySpec = new SecretKeySpec(encryptKey.getBytes(), "AES");
            // Use AES in GCM mode with no padding
            Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
            // Set up the GCM parameter specification with the provided IV
            GCMParameterSpec ivSpec = new GCMParameterSpec(16 * Byte.SIZE, iv.getBytes("UTF-8"));
            // Initialize the cipher for encryption
            cipher.init(Cipher.ENCRYPT_MODE, keySpec, ivSpec);
            // Perform the encryption
            byte[] encryptData = cipher.doFinal(srcString.getBytes("UTF-8"));
            // Return the encrypted data as a hexadecimal string
            return parseByte2HexStr(encryptData);
        } catch (Exception ex) {
            throw new Exception("encryptAES", ex);
        }
    }

    /**
     * Decrypts a string that was encrypted using AES encryption with GCM mode.
     * @param encryptedStr The encrypted string in hexadecimal format.
     * @param decryptKey The key used for decryption.
     * @param iv The initialization vector used for decryption.
     * @return The decrypted plaintext string.
     * @throws Exception If decryption fails.
     */
    public static String decryptAES(String encryptedStr, String decryptKey, String iv) throws Exception {
        try {
            // Create a key specification for the decryption key
            SecretKeySpec keySpec = new SecretKeySpec(decryptKey.getBytes(), "AES");
            // Use AES in GCM mode with no padding
            Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
            // Set up the IV parameter specification
            IvParameterSpec ivSpec = new IvParameterSpec(iv.getBytes("UTF-8"));
            // Initialize the cipher for decryption
            cipher.init(Cipher.DECRYPT_MODE, keySpec, ivSpec);
            // Convert the hexadecimal string back to a byte array
            byte[] encryptedBytes = parseHexStr2Byte(encryptedStr);
            // Perform the decryption
            byte[] result = cipher.doFinal(encryptedBytes);
            // Return the decrypted string
            return new String(result, "UTF-8");
        } catch (Exception ex) {
            throw new Exception("decryptAES", ex);
        }
    }

    /**
     * Converts a byte array into a hexadecimal string.
     * @param buf The byte array to convert.
     * @return The resulting hexadecimal string.
     */
    public static String parseByte2HexStr(byte buf[]) {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < buf.length; i++) {
            String hex = Integer.toHexString(buf[i] & 0xFF);
            if (hex.length() == 1) {
                hex = '0' + hex;  // Pad with a leading zero if needed
            }
            sb.append(hex.toUpperCase());  // Convert to uppercase
        }
        return sb.toString();
    }

    /**
     * Converts a hexadecimal string into a byte array.
     * @param hexStr The hexadecimal string to convert.
     * @return The resulting byte array.
     */
    public static byte[] parseHexStr2Byte(String hexStr) {
        if (hexStr.length() < 1) return null;
        byte[] result = new byte[hexStr.length() / 2];
        for (int i = 0; i < hexStr.length() / 2; i++) {
            int high = Integer.parseInt(hexStr.substring(i * 2, i * 2 + 1), 16);
            int low = Integer.parseInt(hexStr.substring(i * 2 + 1, i * 2 + 2), 16);
            result[i] = (byte) (high * 16 + low);
        }
        return result;
    }
}
