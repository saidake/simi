package com.saidake.common.core.util.encode;

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
     * 默认的签名算法“SHA1WithRSA”
     * “SHA1WithRSA”算法，采用同一个私钥签名同一段文字，每次所得的签名都一样。
     */
    public static final String SIGN_ALGORITHMS = "SHA1WithRSA";
    /**
     * “SHA256WithRSA”算法，采用同一个私钥签名同一段文字，每次所得的签名都一样。
     */
    public static final String SIGN_ALGORITHMS_SHA256 = "SHA256WithRSA";

    /**
     *
     *
     * @param signText
     * @param keyFileName
     * @return
     * @throws Exception
     */
    public String sign(String signText, String keyFileName, String keyPassword)
            throws Exception {
        try {
            byte[] keyBytes = loadKey(keyFileName, keyPassword);//????????????????????
            PKCS8EncodedKeySpec pkcs8KeySpec = new PKCS8EncodedKeySpec(keyBytes);
            KeyFactory keyFactory = KeyFactory.getInstance("RSA");
            Signature signature = Signature.getInstance("SHA256withRSA");
            signature.initSign(keyFactory.generatePrivate(pkcs8KeySpec));
            signature.update(signText.getBytes("UTF-8"));
            byte[] result = signature.sign();
            return Base64.encodeBase64String(result);
        } catch (Exception ex) {
            throw new Exception("加密失败", ex);
        }
    }


    /**
     * ???????
     *
     * @param keyFileName ????????
     * @param keyPassword ??????????????????????????????????????
     * @return
     * @throws NoSuchAlgorithmException
     * @throws Exception
     */
    private byte[] loadKey(String keyFileName, String keyPassword)
            throws NoSuchAlgorithmException, Exception {

        String signKey;
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(URLDecoder.decode(keyFileName, "UTF-8")));
            String readLine = null;
            StringBuilder sb = new StringBuilder();
            while ((readLine = reader.readLine()) != null) {
                sb.append(readLine);
            }

            if (keyPassword != null) {
                Key desKey = DESUtil.generateKey(keyPassword);
                signKey = DESUtil.decryptDES(sb.toString(), desKey);
            } else {
                signKey = sb.toString();
            }
        } finally {
            if (reader != null) {
                reader.close();
            }
        }
        return Base64.decodeBase64(signKey);
    }


    public void verify(String verifiText, String signedText, String keyFileName, String keyPassword) throws Exception {

        boolean verifyResult;
        try {
            byte[] keyBytes = loadKey(keyFileName, keyPassword);

            X509EncodedKeySpec keySpec = new X509EncodedKeySpec(keyBytes);
            KeyFactory keyFactory = KeyFactory.getInstance("RSA");
            Signature signature = Signature.getInstance("SHA256withRSA");
            signature.initVerify(keyFactory.generatePublic(keySpec));
            signature.update(verifiText.getBytes("UTF-8"));
            byte[] verifyBytes = Base64.decodeBase64(signedText);// ?????? org.apache.commons.codec.binary.Base64;
            verifyResult = signature.verify(verifyBytes);
        } catch (Exception ex) {
            throw new Exception("验证参数[" + verifiText + "加密参数" + signedText + "]", ex);
        }
        if (!verifyResult) {
            throw new Exception("验证失败");
        }
    }


    public static PublicKey getPublicKey(String key) throws Exception {
        byte[] keyBytes;
//        keyBytes = (new BASE64Decoder()).decodeBuffer(key);
        keyBytes = Base64.decodeBase64(key);
        X509EncodedKeySpec keySpec = new X509EncodedKeySpec(keyBytes);
        KeyFactory keyFactory = KeyFactory.getInstance("RSA");
        PublicKey publicKey = keyFactory.generatePublic(keySpec);
        return publicKey;
    }

    public static PrivateKey getPrivateKey(String key) throws Exception {
        byte[] keyBytes;
//        keyBytes = (new BASE64Decoder()).decodeBuffer(key);
        keyBytes = Base64.decodeBase64(key);
        PKCS8EncodedKeySpec keySpec = new PKCS8EncodedKeySpec(keyBytes);
        KeyFactory keyFactory = KeyFactory.getInstance("RSA");
        PrivateKey privateKey = keyFactory.generatePrivate(keySpec);
        return privateKey;
    }


    /**
     * 采用的特定的签名算法，通过公钥进行RSA验证签名
     * @param content 待签名数据
     * @param sign 经过BASE64编码之后的签名值
     * @param publicKey 公钥
     * @param signAlgorithm 签名算法
     * @return 布尔值，true表示签名一致，false表示签名不一致
     */
    public static boolean checkSign(String content, String sign, PublicKey publicKey, String signAlgorithm, String charsetName) throws Exception{
        Signature signature = Signature.getInstance(signAlgorithm, PROVIDER);
        signature.initVerify(publicKey);
        signature.update(content.getBytes(charsetName));
        return signature.verify(decodeBASE64(sign));
    }

    /**
     * 采用默认的签名算法SHA1WithRSA，通过公钥进行RSA验证签名
     * 默认字符集编码为utf-8
     * @param content 待签名数据
     * @param sign 经过BASE64编码之后的签名值
     * @param publicKey 公钥
     * @return 布尔值，true表示签名一致，false表示签名不一致
     * @throws Exception
     */
    public static boolean checkSign(String content, String sign, PublicKey publicKey) throws Exception{
        return checkSign(content, sign, publicKey, SIGN_ALGORITHMS, "utf-8");
    }
    /**
     * 采用默认的签名算法SHA256WithRSA，通过公钥进行RSA验证签名
     * 默认字符集编码为utf-8
     * @param content 待签名数据
     * @param sign 经过BASE64编码之后的签名值
     * @param publicKey 公钥
     * @return 布尔值，true表示签名一致，false表示签名不一致
     * @throws Exception
     */
    public static boolean shaCheckSign(String content, String sign, PublicKey publicKey) throws Exception{
        return checkSign(content, sign, publicKey, SIGN_ALGORITHMS_SHA256, "utf-8");
    }


    /**
     * BASE64编码
     * @param bytes 待编码字节数组
     * @return 编码之后的字符串
     */
    public static String encodeBASE64(byte[] bytes) throws Exception{
        //BASE64Encoder b64=new BASE64Encoder();
        //return b64.encode(bytes);
        return Base64.encodeBase64String(bytes);
    }

    /**
     * BASE64解码
     * @param text 待解码字符串
     * @return 解码之后的字节数组
     * @throws IOException
     */
    public static byte[] decodeBASE64(String text) throws Exception{
        //BASE64Decoder b64=new BASE64Decoder();
        //return b64.decodeBuffer(text);
        return Base64.decodeBase64(text);
    }

}
