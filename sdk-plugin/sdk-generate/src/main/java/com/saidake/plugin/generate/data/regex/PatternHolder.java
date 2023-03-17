package com.saidake.plugin.generate.data.regex;

import java.util.regex.Pattern;

/**
 * 正则管理中心
 */
public interface PatternHolder {
    // "xxx"
    Pattern stringPattern= Pattern.compile("\".*?\"");


    // package com.saidake.dd;
    Pattern packagePattern= Pattern.compile("package\\s*?([A-z0-9$.]*?)\\s*?;");

    // private class TestController {
    Pattern classPattern= Pattern.compile("(public|private|protected)\\s*?(static)?\\s*?class\\s*?([A-z0-9$]*?)\\s*?\\{");

    // @RequestMapping(value="/test")
    Pattern requestMappingPattern = Pattern.compile("@(Get|Post|Request|Delete|Put)Mapping\\(((.*?value\\s*?=\\s*?\"(.*?)\".*?)|(\\s*?\"(.*?)\"\\s*?))\\)");

    // private void lala(
    Pattern methodPattern= Pattern.compile("\\s*?(public|private|protected)\\s*?(static)?\\s*?([A-z0-9?<>\\s.,]*?)\\s*?([A-z0-9$]*?)\\s*?\\(");

    // @Tag(name="/test")
    Pattern springDocTagPattern= Pattern.compile("@Tag\\((.*?name\\s*?=\\s*?\"(.*?)\".*?)\\)");

    // @Operation(summary="/test")
    Pattern springDocOperationPattern= Pattern.compile("@Operation\\((.*?" +
            "((\\s*?(summary|description)\\s*?=\\s*?\"(.*?)\")\\s*?,?)+" +
            ".*?)\\)");

    // test (
    // service.test (
    // String result = test (
    // String result = service.test (
    Pattern callMethodPattern= Pattern.compile(
            "[^@A-z0-9$]+([A-z0-9$]*?\\s*?[A-z0-9$]*?)?" +    // String result
                    "(\\s*?=\\s*?)?"+           //  =
                    "(([A-z0-9$]*?)\\.)?" +     //  service.
                    "([a-z]+[A-z0-9$]*?)" +  // test
                    "\\s*?\\("         // (
    );

}
