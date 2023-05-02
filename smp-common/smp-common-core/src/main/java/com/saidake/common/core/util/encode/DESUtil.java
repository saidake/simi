package com.saidake.common.core.util.encode;

import org.apache.commons.codec.binary.Base64;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;

public class DESUtil {
	/**
     * ����������Կ
     * @param strKey ��������
     * @return
     * @throws NoSuchAlgorithmException
     */
    public static Key generateKey(String strKey) throws NoSuchAlgorithmException {
        KeyGenerator keyGenerator = KeyGenerator.getInstance("AES");
//        KeyGenerator keyGenerator = KeyGenerator.getInstance("DES");
        SecureRandom secureRandom = SecureRandom.getInstance("SHA1PRNG");
        secureRandom.setSeed(strKey.getBytes());
        keyGenerator.init(secureRandom);
        return keyGenerator.generateKey();
    }
    
    
    /**
     * DES����
     * @param encryptedString �����ܵ�����
     * @param key ����key
     * @return
     * @throws Exception
     */
    public static String decryptDES(String encryptedString, Key key) throws Exception {
        byte[] bs = Base64.decodeBase64(encryptedString);
        Cipher cipher = Cipher.getInstance("AES");
//        Cipher cipher = Cipher.getInstance("DES");
        cipher.init(Cipher.DECRYPT_MODE, key);
        byte[] cipherBytes = cipher.doFinal(bs);
        return new String(cipherBytes, "UTF-8");
    }
}
