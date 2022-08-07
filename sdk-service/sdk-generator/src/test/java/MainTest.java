import com.saidake.common.core.util.data.RandomUtil;
import com.saidake.common.core.util.data.StringUtil;
import com.saidake.common.core.util.file.FileUtil;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class MainTest {
    public static void main(String[] args) throws ParseException {
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
        int totalTimes=365;  // 总天数
        int totalCount=0;
        int limitCount=3;    //当日限制交易次数
        int money=10;        //一单盈利或亏损
        int failedPercent=40;  //亏损概率
        int successPercent=60;  //盈利概率

        int lossMoney=0;          //总亏损
        int makeMoney=0;          //总盈利
        int makeMoneyTimes=0;
        int lossMoneyTimes=0;
        int makeMoneyDayTimes=0;
        int lossMoneyDayTimes=0;
        for (int i = 0; i < totalTimes; i++) {
            int sum=0;
            int count=0;
            while(sum<=0&&count<limitCount){
                int temp=RandomUtil.getRandomNum(-failedPercent,successPercent+1);  //[-21,20]
                while(temp==0){
                    temp=RandomUtil.getRandomNum(-failedPercent,successPercent+1);  //[-21,20]
                }
                if(temp>0){
                    sum+=money;
                }else{
                    sum-=money;
                }
                count++;
            }
            if(count>=limitCount&&sum<=0){
                lossMoney+=sum;
                lossMoneyTimes+=limitCount;
                lossMoneyDayTimes+=1;
            }
            if(sum>0){
                makeMoney+=sum;
                makeMoneyTimes+=1;
                makeMoneyDayTimes+=1;
            }
            totalCount+=count;
        }
        System.out.println(totalCount/totalTimes);
        System.out.println("盈利天数："+makeMoneyDayTimes);
        System.out.println("亏损天数："+lossMoneyDayTimes);
        System.out.println("盈利次数："+makeMoneyTimes);
        System.out.println("亏损次数："+lossMoneyTimes);
        System.out.println("总盈利："+makeMoney);
        System.out.println("总亏损："+lossMoney);

        System.out.println(String.format("胜率：%.2f",-(double)successPercent/(successPercent+failedPercent)));
        System.out.println(String.format("盈利比：%.2f",(double)makeMoney/(makeMoney-lossMoney)));
        System.out.println(String.format("盈亏比：%.2f",-(double)makeMoney/lossMoney));
    }

}
