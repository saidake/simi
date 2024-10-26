package com.simi.sgz.utils;


import com.github.kwhat.jnativehook.GlobalScreen;
import com.github.kwhat.jnativehook.keyboard.NativeKeyEvent;
import com.github.kwhat.jnativehook.keyboard.NativeKeyListener;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class JNativeUtils {
    public static void main(String[] args) throws InterruptedException {
        Thread mainThread = Thread.currentThread();
        //setupGlobalKeyEventListener();
        while (true){
            Thread.sleep(1000);
        }
    }
    public static void setupGlobalKeyEventListener(int keyCode, Runnable executor, String message) {
        log.info(message);
        try {
            // Register the native hook
            GlobalScreen.registerNativeHook();
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }

        // Add the global key listener
        GlobalScreen.addNativeKeyListener(new NativeKeyListener() {
            @Override
            public void nativeKeyPressed(NativeKeyEvent e) {
                if (e.getKeyCode() == keyCode) {
                    log.info("{} Key pressed.", NativeKeyEvent.getKeyText(e.getKeyCode()));
                    try {
                        executor.run();
                    } catch (Exception ex) {
                        log.error("Key event error: {}",e.getKeyChar());
                        throw new RuntimeException(ex);
                    }
                }
            }
        });
    }
}
