package com.simi.labs.common.utils.encode;

import com.simi.labs.common.utils.data.DateUtil;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtBuilder;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Slf4j
@UtilityClass
public class JwtUtil {
    public interface JwtKeyConstant{
        String BOOS_KEY = "fffffsssssfffffsssssfffffsssssaa";
        String APP_KEY = "f232ffffffffffffff";
        String COMMODITY_KEY = "ssssdfsfdsfsfsf";
    }
    private static final String SUBJECT = "Simi App";

    /**
     * Generates a JWT token after user login.
     * This method uses the HMAC-SHA256 (HS256) algorithm and the user's password as the private key.
     *
     * @param day  the expiration time of the JWT in days
     * @param user the user object that represents the logged-in user
     * @return the encrypted JWT token
     */
    public static String createJWT(Integer day, Map user) {
        // Specify the algorithm used for signing the JWT
        SignatureAlgorithm signatureAlgorithm = SignatureAlgorithm.HS256;

        // Get the current time for JWT generation
        long nowMillis = System.currentTimeMillis();
        Date now = new Date(nowMillis);

        // Create custom claims (additional user data can be added here)
        Map<String, Object> claims = new HashMap<String, Object>();
        claims.put("id", user.get("id"));
        claims.put("mobile", user.get("mobile"));

        // Secret key for signing the JWT, which should be kept private
        String key = JwtKeyConstant.BOOS_KEY;

        // Create the JWT builder with various claims and configurations
        JwtBuilder builder = Jwts.builder()
                .setClaims(claims) // Set custom claims
                .setId(UUID.randomUUID().toString()) // Set a unique JWT ID to prevent replay attacks
                .setIssuedAt(now) // Set the issue date of the JWT
                .setSubject(SUBJECT) // Set the subject (representing the owner of the JWT)
                .signWith(signatureAlgorithm, key); // Sign the JWT with the chosen algorithm and secret key

        // If a valid expiration time is provided, set the expiration date
        if (day >= 0) {
            Date exp = DateUtil.addDateMinutes(now, day);
            builder.setExpiration(exp); // Set the expiration time for the token
        }

        String data = "";
        try {
            // Encrypt the JWT token using AES encryption with the secret key
            data = AESUtil.encryptAES(builder.compact(), key, key);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException("Login encryption failed");
        }

        return data;
    }

    /**
     * Decrypts and parses the JWT token.
     *
     * @param token the encrypted JWT token
     * @return the claims extracted from the JWT token
     */
    public static Claims parseJWT(String token) {
        try {
            // Secret key used for signing and verifying the JWT token
            String key = JwtKeyConstant.BOOS_KEY;

            // Decrypt the token using AES decryption
            token = AESUtil.decryptAES(token, key, key);

            // Parse the JWT token to extract the claims
            Claims claims = Jwts.parser()
                    .setSigningKey(key) // Set the secret key used for verification
                    .parseClaimsJws(token).getBody(); // Parse the claims from the JWT

            return claims;
        } catch (Exception e) {
            log.info("Error while parsing JWT claims");
            return null;
        }
    }

    /**
     * Verifies the JWT token by checking the validity of the claims.
     * This method can also verify if the password matches the one stored in the database.
     *
     * @param token the JWT token to verify
     * @return true if the token is valid, false otherwise
     */
    public static Boolean isVerify(String token) {
        try {
            // Secret key used for signing and verifying the JWT token
            String key = JwtKeyConstant.BOOS_KEY;

            // Decrypt the token using AES decryption
            token = AESUtil.decryptAES(token, key, key);

            // Parse the JWT token to extract the claims
            Claims claims = Jwts.parser()
                    .setSigningKey(key) // Set the secret key used for verification
                    .parseClaimsJws(token).getBody(); // Parse the claims from the JWT

            // Extract the user ID from the claims (can be used for further validation)
            Integer id = (Integer) claims.get("id");
            return true;
        } catch (Exception e) {
            // Token verification failed, return false
            return false;
        }
    }

    /**
     * Extracts the mobile number from the JWT token.
     *
     * @param token the JWT token
     * @return the mobile number associated with the token, or null if an error occurs
     */
    public static String getMobile(String token) {
        try {
            // Secret key used for signing and verifying the JWT token
            String key = JwtKeyConstant.BOOS_KEY;

            // Decrypt the token using AES decryption
            token = AESUtil.decryptAES(token, key, key);

            // Parse the JWT token to extract the claims
            Claims claims = Jwts.parser()
                    .setSigningKey(key) // Set the secret key used for verification
                    .parseClaimsJws(token).getBody(); // Parse the claims from the JWT

            // Extract the mobile number from the claims
            String mobile = (String) claims.get("mobile");
            return mobile;
        } catch (Exception e) {
            log.info("Error while extracting mobile from JWT");
            return null;
        }
    }
}
