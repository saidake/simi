package com.simi.streamtest;

abstract class AbstractSpinedBuffer {
    protected int elementIndex;
    protected int spineIndex;

    /**
     * Count of elements in all prior chunks.
     */
    protected long[] priorElementCount;
}
