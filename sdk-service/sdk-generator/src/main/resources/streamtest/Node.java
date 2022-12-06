package com.saidake.streamtest;

import com.saidake.backup.Spliterator;

interface Node<T>{
    Spliterator<T> spliterator();
}
