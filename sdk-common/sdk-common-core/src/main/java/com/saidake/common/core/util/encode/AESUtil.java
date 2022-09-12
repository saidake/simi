package com.saidake.common.core.util.encode;

import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

public class AESUtil {
    public static String encryptAES(String srcString, String encryptKey, String iv) throws Exception {
        try {
            SecretKeySpec keySpec = new SecretKeySpec(encryptKey.getBytes(), "AES");
            Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
//            Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
            GCMParameterSpec ivSpec = new GCMParameterSpec(16 * Byte.SIZE,iv.getBytes("UTF-8"));
//            IvParameterSpec ivSpec = new IvParameterSpec(iv.getBytes("UTF-8"));
            cipher.init(Cipher.ENCRYPT_MODE, keySpec, ivSpec);
            byte[] encryptData = cipher.doFinal(srcString.getBytes("UTF-8"));
            return parseByte2HexStr(encryptData);
        } catch (Exception ex) {
            throw new Exception("encryptAES", ex);
        }
    }


    public static String decryptAES(String encryptedStr, String decryptKey, String iv) throws Exception {
        try {
            SecretKeySpec keySpec = new SecretKeySpec(decryptKey.getBytes(), "AES");
//            Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
            Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
            IvParameterSpec ivSpec = new IvParameterSpec(iv.getBytes("UTF-8"));
            cipher.init(Cipher.DECRYPT_MODE, keySpec, ivSpec);
            byte[] encryptedBytes = parseHexStr2Byte(encryptedStr);
            byte[] result = cipher.doFinal(encryptedBytes);
            return new String(result, "UTF-8");
        } catch (Exception ex) {
            throw new Exception("decryptAES", ex);
        }
    }


    public static String parseByte2HexStr(byte buf[]) {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < buf.length; i++) {
            String hex = Integer.toHexString(buf[i] & 0xFF);
            if (hex.length() == 1) {
                hex = '0' + hex;
            }
            sb.append(hex.toUpperCase());
        }
        return sb.toString();
    }


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
