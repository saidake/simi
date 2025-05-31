package com.simi.labs.common.utils.data;

import lombok.experimental.UtilityClass;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@UtilityClass
public class StringUtil {
    private static Pattern linePattern = Pattern.compile("_(\\w)");   // Pattern to match underscore followed by a word character
    private static Pattern humpPattern = Pattern.compile("[A-Z]");     // Pattern to match uppercase letters

    /**
     * Counts the occurrences of a substring within a string.
     * @param source The string in which to search for the substring.
     * @param sub The substring to count.
     * @return The number of times the substring appears in the source string.
     */
    public static Integer countSubString(String source, String sub) {
        Integer resultCount = 0;
        for (int i = 0, next = 0; (next = source.indexOf(sub, i)) >= 0; i = next + sub.length()) {
            resultCount++;
        }
        return resultCount;
    }

    /**
     * Converts the first letter of a string to uppercase.
     * @param str The input string.
     * @return The string with the first letter in uppercase.
     * @throws RuntimeException if the input string is empty.
     */
    public static String startUpper(String str) {
        if (str.isEmpty()) {
            throw new RuntimeException("empty str");
        }
        char[] chars = str.toCharArray();
        char start = chars[0];
        if (start >= 'a' && start <= 'z') {
            start = (char) (start - 32);  // Convert to uppercase
        }
        chars[0] = start;
        return String.valueOf(chars);
    }

    /**
     * Converts the first letter of a string to lowercase.
     * @param str The input string.
     * @return The string with the first letter in lowercase.
     */
    public static String startLower(String str) {
        if (str.isEmpty()) {
            return null;
        }
        char[] chars = str.toCharArray();
        char start = chars[0];
        if (start >= 'A' && start <= 'Z') {
            start = (char) (start + 32);  // Convert to lowercase
        }
        chars[0] = start;
        return String.valueOf(chars);
    }

    /**
     * Converts an uppercase string to a camelCase variable format.
     * For example, "CONTACT & CREDIT" becomes "contactCredit".
     * @param str The input string in uppercase.
     * @param isStartUpper If true, the first letter will be uppercase, otherwise lowercase.
     * @return The string in camelCase format.
     */
    public static String convertUpperString(String str, Boolean isStartUpper) {
        String[] split = str.split("[^A-z]+");  // Split string on non-alphabet characters
        List<String> stringList = new ArrayList<>();
        for (int i = 0; i < split.length; i++) {
            if (i == 0 && !isStartUpper) {
                stringList.add(split[i].toLowerCase());
                continue;
            }
            stringList.add(startUpper(split[i].toLowerCase()));
        }
        return StringUtils.join(stringList);  // Join the list into a single string
    }

    /**
     * Converts a camelCase string to an uppercase underscore-separated format.
     * For example, "camelCaseString" becomes "CAMEL_CASE_STRING".
     * @param str The input string in camelCase format.
     * @return The string in uppercase and underscore-separated format.
     */
    public static String humpToLine(String str) {
        Matcher matcher = humpPattern.matcher(str);
        StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            matcher.appendReplacement(sb, "_" + matcher.group(0).toUpperCase());  // Replace each uppercase letter with an underscore and its uppercase version
        }
        matcher.appendTail(sb);
        return sb.toString().toUpperCase();  // Convert the result to uppercase
    }

    /**
     * Converts an underscore-separated string to camelCase format.
     * For example, "camel_case_string" becomes "camelCaseString".
     * @param str The input string in underscore-separated format.
     * @return The string in camelCase format.
     */
    public static String lineToHump(String str) {
        Matcher matcher = linePattern.matcher(str);
        StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            matcher.appendReplacement(sb, matcher.group(1).toUpperCase());  // Replace underscore and following character with its uppercase version
        }
        matcher.appendTail(sb);
        return sb.toString();  // Return the transformed string
    }
}
