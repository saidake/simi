package com.saidake.test.file;


import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.UUID;

public class FileTest {
    public static void main(String[] args) throws IOException, ParserConfigurationException, SAXException, TransformerException, NoSuchAlgorithmException {
//        SmpInit.init();
        UUID uuid = UUID.randomUUID();
        System.out.println(UUID.randomUUID());
        System.out.println(UUID.fromString(uuid.toString()));
        System.out.println(UUID.nameUUIDFromBytes("7e901d77-b901-4585-8c8f-0a50174d3eff".getBytes(StandardCharsets.UTF_8)));
        System.out.println(UUID.nameUUIDFromBytes("7e901d77-b901-4585-8c8f-0a50174d3eff".getBytes(StandardCharsets.UTF_8)));
        System.out.println(UUID.fromString("7e901d77-b901-4585-8c8f-0a50174d3eff"));
        System.out.println(UUID.fromString("7e901d77-b901-4585-8c8f-0a50174d3eff"));
        MessageDigest md5 = MessageDigest.getInstance("MD5");
        byte[] digest = md5.digest("C:\\Users\\saidake\\Desktop\\DevProject\\saidake-manage-project\\README.md".getBytes(StandardCharsets.UTF_8));
        System.out.println(new String(digest));
        System.out.println(digest[0]);
        System.out.println(digest[1]);
        System.out.println(Character.forDigit((digest[0] & 240) >> 4, 16));;
        System.out.println(Character.forDigit(digest[2] & 15, 16));
        System.out.println(digest[2]);
        byte[] digest1 = md5.digest("C:\\Users\\saidake\\Desktop\\DevProject\\saidake-manage-project\\README.md".getBytes(StandardCharsets.UTF_8));
        System.out.println(new String(digest1));
        System.out.println(digest1[0]);
        System.out.println(digest1[1]);
        System.out.println(digest1[2]);
        byte[] digest2= md5.digest("C:\\Users\\saidake\\Desktop\\DevProject\\saidake-manage-project\\README.md2".getBytes(StandardCharsets.UTF_8));
        System.out.println(new String(digest2));
        System.out.println(digest2[0]);
        System.out.println(digest2[0]);
        System.out.println(digest2[0]);

    }
}
