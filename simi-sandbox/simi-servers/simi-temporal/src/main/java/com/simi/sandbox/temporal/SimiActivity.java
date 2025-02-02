package com.simi.sandbox.temporal;

import io.temporal.activity.ActivityInterface;

@ActivityInterface
public interface SimiActivity {
    String performTask(String input);
}
