package com.saidake.common.core.util.encode;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

@Slf4j
public class MD5Util {
    public static final int MD532=32;
    public static final int MD516=16;
    private static final Logger logger = LoggerFactory.getLogger(MD5Util.class);

    /**
     * 获取MD5消息摘要
     *
     * @param data 源数据
     * @return MD5消息摘要
     */
    public static byte[] getMD5(byte[] data) {
        byte[] md5 = null;
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            md5 = md.digest(data);
        } catch (Exception e) {
        }
        return md5;
    }

    /*
     * MD5字符串
     */
    public static String md5(String str) {
        MessageDigest messageDigest = null;
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
            logger.error(e.getMessage());
        } catch (UnsupportedEncodingException e) {
            logger.error(e.getMessage());
        }

        return md5StrBuff.toString();
    }

    /*
     * MD5字符串
     */
    public static String getMD5StrLower(String str) {
        MessageDigest messageDigest = null;
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
            logger.error(e.getMessage());
        } catch (UnsupportedEncodingException e) {
            logger.error(e.getMessage());
        } catch (Exception e){
            logger.error(e.getMessage());
        }


        return md5StrBuff.toString();
    }

    /*
     * MD5字符串
     */
    public static String getMD5Str(byte[] data) {
        MessageDigest messageDigest = null;
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
            logger.error("NoSuchAlgorithmException = " + e.toString());
        } catch (Exception e){
            logger.error(e.getMessage());
        }


        return md5StrBuff.toString().toUpperCase();
    }

}
