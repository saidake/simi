package com.saidake.streamtest;

import java.util.function.Supplier;

interface TerminalSink<T, R> extends Sink<T>, Supplier<R> {
}
