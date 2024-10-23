package com.simi.sgz.utils;


import com.github.kwhat.jnativehook.GlobalScreen;
import com.github.kwhat.jnativehook.keyboard.NativeKeyEvent;
import com.github.kwhat.jnativehook.keyboard.NativeKeyListener;

import java.util.Collection;
import java.util.List;

public class JNativeUtils {
    public static void main(String[] args) throws InterruptedException {
        Thread mainThread = Thread.currentThread();
        setupGlobalKeyEventListener(List.of(mainThread));
        while (true){
            Thread.sleep(1000);
        }
    }

    public static void setupGlobalKeyEventListener(Collection<Thread> threadList) {
        System.out.println("Press 'P' to interrupt.");
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
                if (e.getKeyCode() == NativeKeyEvent.VC_P) {
                    System.out.println("P key pressed. Interrupting threads.");
                    for (Thread thread : threadList) {
                        thread.interrupt();
                    }
                }
            }
        });
    }
}
