package com.saidake.common.core.util.file.support.controller;

import java.util.regex.Pattern;

public interface ControllerPattern {
    // "xxx"
    Pattern STRING_PATTERN= Pattern.compile("\".*?\"");

    /**
     * package com.saidake.dd;
     * 1: com.saidake.dd
     */
    Pattern PACKAGE_PATTERN= Pattern.compile("package\\s*?([A-z0-9$.]*?)\\s*?;");

    // private class TestController {
    Pattern CLASS_PATTERN= Pattern.compile("(public|private|protected)\\s*?(static)?\\s*?class\\s*?([A-z0-9$]*?)\\s*?\\{");

    // @RequestMapping(value="/test")
    Pattern REQUEST_MAPPING_PATTERN = Pattern.compile("@(Get|Post|Request|Delete|Put)Mapping\\(((.*?value\\s*?=\\s*?\"(.*?)\".*?)|(\\s*?\"(.*?)\"\\s*?))\\)");

    // private void lala(
    Pattern METHOD_PATTERN= Pattern.compile("\\s*?(public|private|protected)\\s*?(static)?\\s*?([A-z0-9?<>\\s.,]*?)\\s*?([A-z0-9$]*?)\\s*?\\(");

    // @Tag(name="/test")
    Pattern SPRING_DOC_TAG_PATTERN= Pattern.compile("@Tag\\((.*?name\\s*?=\\s*?\"(.*?)\".*?)\\)");

    // @Operation(summary="/test")
    Pattern SPRING_DOC_OPERATION_PATTERN= Pattern.compile("@Operation\\((.*?" +
            "((\\s*?(summary|description)\\s*?=\\s*?\"(.*?)\")\\s*?,?)+" +
            ".*?)\\)");

    // test (
    // service.test (
    // String result = test (
    // String result = service.test (
    Pattern CALL_METHOD_PATTERN= Pattern.compile(
            "[^@A-z0-9$]+([A-z0-9$]*?\\s*?[A-z0-9$]*?)?" +    // String result
                    "(\\s*?=\\s*?)?"+           //  =
                    "(([A-z0-9$]*?)\\.)?" +     //  service.
                    "([a-z]+[A-z0-9$]*?)" +  // test
                    "\\s*?\\("         // (
    );

}
