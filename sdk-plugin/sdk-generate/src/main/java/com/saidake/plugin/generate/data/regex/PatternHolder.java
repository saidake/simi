package com.saidake.plugin.generate.data.regex;


import java.util.regex.Pattern;

public interface PatternHolder {
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
}
