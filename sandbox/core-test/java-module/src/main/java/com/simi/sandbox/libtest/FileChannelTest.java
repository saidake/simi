package com.simi.sandbox.libtest;

import java.io.FileOutputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class FileChannelTest {
    public static void main(String[] args) {
        ExecutorService executorService = Executors.newFixedThreadPool(4);
        executorService.submit(FileChannelTest::writeFile);
        executorService.submit(FileChannelTest::writeFile);
        executorService.submit(FileChannelTest::writeFile);
        executorService.shutdown();
    }

    private static void writeFile() {
        System.out.println(Thread.currentThread());
        try (FileOutputStream fos = new FileOutputStream("C:\\Users\\simi\\Desktop\\DevProjects\\simi\\simi-sandbox\\simi-aaa\\simi-lib-test\\src\\main\\resources\\test.csv", true); // Open in append mode
             FileChannel fileChannel = fos.getChannel()) {

            // Acquire an exclusive lock
            FileLock lock = fileChannel.lock();

            try {
                // Prepare data to write
                ByteBuffer buffer = ByteBuffer.wrap(("test1,test2"+System.lineSeparator()).getBytes());
                // Write data to the file
                fileChannel.write(buffer);
                System.out.println("Data written to the file.");
            } finally {
                // Release the lock
                lock.release();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
