package com.saidake.streamtest;

interface Node<T>{
    Spliterator<T> spliterator();
}
