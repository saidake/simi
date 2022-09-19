import com.saidake.common.core.util.data.RandomUtil;
import org.apache.commons.lang3.StringUtils;
import org.springframework.core.LocalVariableTableParameterNameDiscoverer;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.Expression;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;

import java.util.HashMap;


public class MainTest {
    public static void main(String[] args) {
        //=============================================================================================== 测试样本
        String testString="abcdefg12345";

        //=============================================================================================== 实验区
//        System.out.println(StringUtils.join(testString.chars().mapToObj(c->(char)c).toArray(Character[]::new),"\u200b"));
//        System.out.println(RandomUtil.getRandomLong(100L,300L));

        // SPEL
        EvaluationContext context = new StandardEvaluationContext();
        context.setVariable("id", 233);
        context.setVariable("lala", "fff");
        context.setVariable("map",new HashMap<String,String>(){{put("name","kk");}});
        SpelExpressionParser spelExpressionParser = new SpelExpressionParser();
        Expression expression = spelExpressionParser.parseExpression("'test value: '+#map.get('name')");
        System.out.println(expression.getValue(context, String.class));;
    }

}
