package com.saidake.plugin.generate.util;


import com.saidake.plugin.generate.util.support.controller.ControllerPattern;
import lombok.experimental.UtilityClass;

import java.util.Optional;
import java.util.regex.Matcher;

@UtilityClass
public class SmpControllerUtils {

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
