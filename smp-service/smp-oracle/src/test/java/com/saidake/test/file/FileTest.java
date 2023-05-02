package com.saidake.test.file;

import com.saidake.common.core.util.file.SmpFileUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.EmptyFileFilter;

import java.io.File;
import java.io.IOException;
import java.util.Collection;

public class FileTest {
    public static void main(String[] args) throws IOException {
        SmpFileUtils.readAndWriteStringFile(
               "D:\\Desktop\\DevProject\\saidake-manage-project\\smp-service\\smp-oracle\\src\\test\\java\\com\\saidake\\dashboard.txt",
               "D:\\Desktop\\DevProject\\saidake-manage-project\\smp-service\\smp-oracle\\src\\test\\java\\com\\saidake\\dashboard.txt",
                string->string+"ddd"
        );
        SmpFileUtils.readAndPutAllProperties("D:\\Desktop\\DevProject\\saidake-manage-project\\smp-service\\smp-oracle\\src\\test\\java\\com\\saidake\\target.properties",
                "D:\\Desktop\\DevProject\\saidake-manage-project\\smp-service\\smp-oracle\\src\\test\\java\\com\\saidake\\source.properties"
                );
        Collection<File> files = FileUtils.listFiles(new File("D:\\Desktop\\DevProject\\saidake-manage-project\\smp-service\\smp-oracle\\src\\test\\java\\com\\saidake\\test\\db"),
                EmptyFileFilter.NOT_EMPTY, null);
        files.forEach(System.out::println);
    }
}
