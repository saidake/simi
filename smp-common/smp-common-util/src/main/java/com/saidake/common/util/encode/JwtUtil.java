package com.saidake.common.util.encode;

import com.saidake.common.util.data.DateUtil;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtBuilder;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import lombok.extern.slf4j.Slf4j;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Slf4j
public class JwtUtil {
    public interface JwtKeyConstant{
        String BOOS_KEY = "fffffsssssfffffsssssfffffsssssaa";
        String APP_KEY = "f232ffffffffffffff";
        String COMMODITY_KEY = "ssssdfsfdsfsfsf";
    }
    private static final String SUBJECT = "蓝牙app";

    /**
     * 用户登录成功后生成Jwt
     * 使用Hs256算法  私匙使用用户密码
     *
     * @param day  jwt过期时间
     * @param user 登录成功的user对象
     * @return
     */
    public static String createJWT(Integer day, Map user) {
        //指定签名的时候使用的签名算法，也就是header那部分，jjwt已经将这部分内容封装好了。
        SignatureAlgorithm signatureAlgorithm = SignatureAlgorithm.HS256;

        //生成JWT的时间
        long nowMillis = System.currentTimeMillis();
        Date now = new Date(nowMillis);

        //创建payload的私有声明（根据特定的业务需要添加，如果要拿这个做验证，一般是需要和jwt的接收方提前沟通好验证方式的）
        Map<String, Object> claims = new HashMap<String, Object>();
        claims.put("id", user.get("id"));
        claims.put("mobile", user.get("mobile"));
        //生成签名的时候使用的秘钥secret,这个方法本地封装了的，一般可以从本地配置文件中读取，切记这个秘钥不能外露哦。它就是你服务端的私钥，在任何场景都不应该流露出去。一旦客户端得知这个secret, 那就意味着客户端是可以自我签发jwt了。
        String key = JwtKeyConstant.BOOS_KEY;

        //生成签发人
        //String subject = SUBJECT;
        //下面就是在为payload添加各种标准声明和私有声明了
        //这里其实就是new一个JwtBuilder，设置jwt的body
        JwtBuilder builder = Jwts.builder()
                //如果有私有声明，一定要先设置这个自己创建的私有的声明，这个是给builder的claim赋值，一旦写在标准的声明赋值之后，就是覆盖了那些标准的声明的
                .setClaims(claims)
                //设置jti(JWT ID)：是JWT的唯一标识，根据业务需要，这个可以设置为一个不重复的值，主要用来作为一次性token,从而回避重放攻击。
                .setId(UUID.randomUUID().toString())
                //iat: jwt的签发时间
                .setIssuedAt(now)
                //代表这个JWT的主体，即它的所有人，这个是一个json格式的字符串，可以存放什么userid，roldid之类的，作为什么用户的唯一标志。
                .setSubject(SUBJECT)
                //设置签名使用的签名算法和签名使用的秘钥
                .signWith(signatureAlgorithm, key);
        if (day >= 0) {
            Date exp = DateUtil.addDateMinutes(now, day);
            //设置过期时间
            builder.setExpiration(exp);
        }
        String data = "";
        try {
            data = AESUtil.encryptAES(builder.compact(), key, key);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException("登录加密失败");
        }

        return data;
    }


    /**
     * Token的解密
     *
     * @param token 加密后的token
     * @return
     */
    public static Claims parseJWT(String token) {
        //签名秘钥，和生成的签名的秘钥一模一样

        try {
            String key = JwtKeyConstant.BOOS_KEY;
            token = AESUtil.decryptAES(token, key, key);
            //得到DefaultJwtParser
            Claims claims = Jwts.parser()
                    //设置签名的秘钥
                    .setSigningKey(key)
                    //设置需要解析的jwt
                    .parseClaimsJws(token).getBody();
            return claims;
        } catch (Exception e) {
            log.info("encode Claims parseJWT error");
            return null;
        }
    }


    /**
     * 校验token
     * 在这里可以使用官方的校验，我这里校验的是token中携带的密码于数据库一致的话就校验通过
     *
     * @param token
     * @return
     */
    public static Boolean isVerify(String token) {
        try {
            //签名秘钥，和生成的签名的秘钥一模一样
            String key = JwtKeyConstant.BOOS_KEY;
            token = AESUtil.decryptAES(token, key, key);
            //得到DefaultJwtParser
            Claims claims = Jwts.parser()
                    //设置签名的秘钥
                    .setSigningKey(key)
                    //设置需要解析的jwt
                    .parseClaimsJws(token).getBody();
            Integer id = (Integer) claims.get("id");
            return true;
        } catch (Exception e) {
            //e.printStackTrace();
            // encode 校验失败, 抛出Token验证非法异常
            return false;
        }
    }


    public static String getMobile(String token) {
        try {
            //签名秘钥，和生成的签名的秘钥一模一样
            String key = JwtKeyConstant.BOOS_KEY;
            token = AESUtil.decryptAES(token, key, key);
            //得到DefaultJwtParser
            Claims claims = Jwts.parser()
                    //设置签名的秘钥
                    .setSigningKey(key)
                    //设置需要解析的jwt
                    .parseClaimsJws(token).getBody();
            String mobile = (String) claims.get("mobile");
            return mobile;
        } catch (Exception e) {
            //e.printStackTrace();
            // encode 校验失败, 抛出Token验证非法异常
            log.info("encode getMobile error");
            return null;
        }
    }


}