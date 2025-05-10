package com.simi.toolkit.core;


import com.simi.toolkit.core.constants.ControllerPattern;
import lombok.experimental.UtilityClass;

import java.util.Optional;
import java.util.regex.Matcher;

/**
 * Controller file utils.
 */
@UtilityClass
public class SimiControllerUtils {

    public static boolean checkWhetherController(String fileContent){
        return fileContent.contains("@RestController")||fileContent.contains("@Controller");
    }

    public static Optional<String> getPackagePath(String fileContent){
        Matcher packageMatcher= ControllerPattern.PACKAGE_PATTERN.matcher(fileContent);
        if(packageMatcher.find()){
            return Optional.ofNullable(packageMatcher.group(1));
        }else return Optional.empty();
    }

}
