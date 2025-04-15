package com.simi.sandbox.temporal.impl;

import com.simi.sandbox.temporal.SimiActivity;

public class SimiActivityImpl implements SimiActivity {
    @Override
    public String performTask(String input) {
        return "Task completed with input: " + input;
    }
}
