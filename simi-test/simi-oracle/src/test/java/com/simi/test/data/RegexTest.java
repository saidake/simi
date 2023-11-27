package com.simi.test.data;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexTest {

    public static void main(String[] args) {
        Matcher matcher = callMethodPattern.matcher(testLongStr);
        while (matcher.find()){
            System.out.println(matcher.group(5));
        }
    }
    static Pattern testCompile = Pattern.compile("\\(.*?\\)");
    static String testStr="fdfdfdf(dddd),(ttt),ddfsfd'(func)dffff";
    static String testLongStr="Page page, GenFormConf formConf) {\n" +
            "\t\treturn R.ok(genRecordService.page(page, Wrappers.query(formConf)));\n" +
            "\t}\n" +
            "\n" +
            "\t/**\n" +
            "\t * 通过id查询生成记录\n" +
            "\t * @param id id\n" +
            "\t * @return R\n" +
            "\t */\n" +
            "\t@Operation(summary = \"通过id查询\", description = \"通过id查询\")\n" +
            "\t@GetMapping(\"/{id}\")";
    static Pattern callMethodPattern= Pattern.compile(
            "[^@A-z0-9$]+([A-z0-9$]*?\\s*?[A-z0-9$]*?)?" +    // String result
                    "(\\s*?=\\s*?)?"+           //  =
                    "(([A-z0-9$]*?)\\.)?" +     //  service.
                    "([a-z]+[A-z0-9$]*?)" +  // test
                    "\\s*?\\("         // (
    );
}
