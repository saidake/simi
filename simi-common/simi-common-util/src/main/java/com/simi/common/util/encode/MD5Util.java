package com.simi.common.util.encode;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

@Slf4j
public class MD5Util {
    public static final int MD532 = 32;
    public static final int MD516 = 16;
    private static final Logger logger = LoggerFactory.getLogger(MD5Util.class);

    /**
     * Generates the MD5 message digest for the provided data.
     *
     * @param data The input data as a byte array
     * @return The MD5 message digest as a byte array
     */
    public static byte[] getMD5(byte[] data) {
        byte[] md5 = null;
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            md5 = md.digest(data);
        } catch (Exception e) {
            log.error("Error generating MD5 digest", e);
        }
        return md5;
    }

    /**
     * Generates the MD5 hash of a given string and returns it as a hexadecimal string.
     *
     * @param str The input string to be hashed
     * @return The MD5 hash as a hexadecimal string
     */
    public static String md5(String str) {
        MessageDigest messageDigest;
        StringBuffer md5StrBuff = new StringBuffer();

        try {
            messageDigest = MessageDigest.getInstance("MD5");
            messageDigest.reset();
            if (!StringUtils.isEmpty(str)) {
                messageDigest.update(str.getBytes("UTF-8"));
            }
            byte[] byteArray = messageDigest.digest();
            for (int i = 0; i < byteArray.length; i++) {
                if (Integer.toHexString(0xFF & byteArray[i]).length() == 1)
                    md5StrBuff.append("0").append(Integer.toHexString(0xFF & byteArray[i]));
                else
                    md5StrBuff.append(Integer.toHexString(0xFF & byteArray[i]));
            }
        } catch (NoSuchAlgorithmException e) {
            logger.error("Error during MD5 hashing (NoSuchAlgorithmException): " + e.getMessage());
        } catch (UnsupportedEncodingException e) {
            logger.error("Error during MD5 hashing (UnsupportedEncodingException): " + e.getMessage());
        }

        return md5StrBuff.toString();
    }

    /**
     * Generates the MD5 hash of a given string and returns it as a lowercase hexadecimal string.
     *
     * @param str The input string to be hashed
     * @return The MD5 hash as a lowercase hexadecimal string
     */
    public static String getMD5StrLower(String str) {
        MessageDigest messageDigest;
        StringBuffer md5StrBuff = new StringBuffer();
        try {
            messageDigest = MessageDigest.getInstance("MD5");
            messageDigest.reset();
            if (!StringUtils.isEmpty(str)) {
                messageDigest.update(str.getBytes("UTF-8"));
            }
            byte[] byteArray = messageDigest.digest();
            for (int i = 0; i < byteArray.length; i++) {
                if (Integer.toHexString(0xFF & byteArray[i]).length() == 1)
                    md5StrBuff.append("0").append(Integer.toHexString(0xFF & byteArray[i]));
                else
                    md5StrBuff.append(Integer.toHexString(0xFF & byteArray[i]));
            }
        } catch (NoSuchAlgorithmException e) {
            logger.error("Error during MD5 hashing (NoSuchAlgorithmException): " + e.getMessage());
        } catch (UnsupportedEncodingException e) {
            logger.error("Error during MD5 hashing (UnsupportedEncodingException): " + e.getMessage());
        } catch (Exception e) {
            logger.error("Error during MD5 hashing (Exception): " + e.getMessage());
        }

        return md5StrBuff.toString();
    }

    /**
     * Generates the MD5 hash of the provided byte array and returns it as an uppercase hexadecimal string.
     *
     * @param data The input data as a byte array
     * @return The MD5 hash as an uppercase hexadecimal string
     */
    public static String getMD5Str(byte[] data) {
        MessageDigest messageDigest;
        StringBuffer md5StrBuff = new StringBuffer();
        try {
            messageDigest = MessageDigest.getInstance("MD5");
            messageDigest.reset();
            messageDigest.update(data);
            byte[] byteArray = messageDigest.digest();
            for (int i = 0; i < byteArray.length; i++) {
                if (Integer.toHexString(0xFF & byteArray[i]).length() == 1)
                    md5StrBuff.append("0").append(Integer.toHexString(0xFF & byteArray[i]));
                else
                    md5StrBuff.append(Integer.toHexString(0xFF & byteArray[i]));
            }
        } catch (NoSuchAlgorithmException e) {
            logger.error("Error during MD5 hashing (NoSuchAlgorithmException): " + e.getMessage());
        } catch (Exception e) {
            logger.error("Error during MD5 hashing (Exception): " + e.getMessage());
        }

        return md5StrBuff.toString().toUpperCase();
    }
}
