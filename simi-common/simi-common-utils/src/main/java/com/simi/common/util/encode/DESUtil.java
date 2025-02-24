package com.simi.common.util.encode;

import org.apache.commons.codec.binary.Base64;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;

public class DESUtil {
    /**
     * Generates a secret key using the specified seed.
     * @param strKey The seed for key generation.
     * @return The generated key.
     * @throws NoSuchAlgorithmException If the algorithm is not available.
     */
    public static Key generateKey(String strKey) throws NoSuchAlgorithmException {
        // Use AES key generator (replace with DES if necessary)
        KeyGenerator keyGenerator = KeyGenerator.getInstance("AES");
        // KeyGenerator keyGenerator = KeyGenerator.getInstance("DES");
        SecureRandom secureRandom = SecureRandom.getInstance("SHA1PRNG");
        secureRandom.setSeed(strKey.getBytes());
        keyGenerator.init(secureRandom);
        return keyGenerator.generateKey();
    }

    /**
     * Decrypts an encrypted string using the provided key.
     * @param encryptedString The encrypted string to decrypt.
     * @param key The key used for decryption.
     * @return The decrypted plaintext string.
     * @throws Exception If decryption fails.
     */
    public static String decryptDES(String encryptedString, Key key) throws Exception {
        // Decode the Base64 encoded encrypted string
        byte[] bs = Base64.decodeBase64(encryptedString);
        // Use AES cipher for decryption (replace with DES if necessary)
        Cipher cipher = Cipher.getInstance("AES");
        // Cipher cipher = Cipher.getInstance("DES");
        cipher.init(Cipher.DECRYPT_MODE, key);
        // Perform decryption
        byte[] cipherBytes = cipher.doFinal(bs);
        // Return the decrypted string as UTF-8
        return new String(cipherBytes, "UTF-8");
    }
}
