package com.saidake.jar.initializer;

import cn.hutool.core.lang.Assert;
import com.simi.toolkit.core.SimiInitializer;
import com.simi.toolkit.core.exception.InitException;
import org.dom4j.DocumentException;

import java.io.IOException;
import java.util.Optional;

public class SimiInitializerApp {
    public static void main(String[] args) throws DocumentException, InitException, IOException {
        Assert.isTrue(args.length>=2,"arguments length must be greater than or equal to 2");
        SimiInitializer.init(SimiInitializer.readYmlProperties(),args[0],args[1], Optional::empty);
    }
}