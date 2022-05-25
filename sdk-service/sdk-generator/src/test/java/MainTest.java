import com.saidake.common.core.util.data.RandomUtil;
import com.saidake.common.core.util.data.StringUtil;
import com.saidake.common.core.util.encode.JwtUtil;
import com.saidake.common.core.util.file.FileUtil;
import org.apache.commons.lang3.StringUtils;

import java.util.HashMap;

public class MainTest {
    public static void main(String[] args){
//======================================================================================================= 密钥测试
//        String jwtToken = JwtUtil.createJWT(60, new HashMap() {{
//            put("id", 23);
//            put("mobile", "fffffff");
//        }});
//        System.out.println(jwtToken);

//        try {
//            String sss = AESUtil.encryptAES("test", "fffffsssssfffffsssssfffffsssssaa", "keyiv");
//            System.out.println(sss);
//        } catch (Exception e) {
//            e.printStackTrace();
//        }
//======================================================================================================= 字符串测试
        System.out.println(StringUtils.join("aaa","bbb","ccc"));
        System.out.println(FileUtil.joinPath("aaa","\\bbb\\","\\ccccc"));
//        System.out.println(1||"99999");
        String test="abcdddd";
        System.out.println(test.indexOf("abc",99));
        System.out.println(RandomUtil.getRandomName());
        System.out.println(StringUtil.countSubString("{}{}{lalallal","}"));
        System.out.println(RandomUtil.getRandomNumFromString("99不开启1三分钟2五分钟3十分钟]"));
    }
}
