import com.saidake.common.core.util.data.RandomUtil;
import org.apache.commons.lang3.StringUtils;


public class MainTest {
    public static void main(String[] args) {
        //=============================================================================================== 测试样本
        String testString="abcdefg12345";

        //=============================================================================================== 实验区
//        System.out.println(StringUtils.join(testString.chars().mapToObj(c->(char)c).toArray(Character[]::new),"\u200b"));
        System.out.println(RandomUtil.getRandomLong(100L,300L));
    }

}
