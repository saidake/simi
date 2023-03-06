package com.saidake.streamtest;


import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

abstract class ReferencePipeline<P_IN, P_OUT>
        extends AbstractPipeline<P_IN, P_OUT, Stream<P_OUT>>
        implements Stream<P_OUT> {
    @Override
    final StreamShape getOutputShape() {
        return StreamShape.REFERENCE;
    }

    @Override
    @SuppressWarnings("unchecked")
    public final <R> Stream<R> map(Function<? super P_OUT, ? extends R> mapper) {
        Objects.requireNonNull(mapper);
        return new StatelessOp<P_OUT, R>(this, StreamShape.REFERENCE,
                StreamOpFlag.NOT_SORTED | StreamOpFlag.NOT_DISTINCT) {
            @Override
            Sink<P_OUT> opWrapSink(int flags, Sink<R> sink) {
                return new Sink.ChainedReference<P_OUT, R>(sink) {
                    @Override
                    public void accept(P_OUT u) {
                        downstream.accept(mapper.apply(u));
                    }
                };
            }
        };
    }
    abstract static class StatelessOp<E_IN, E_OUT>
            extends ReferencePipeline<E_IN, E_OUT> {
        StatelessOp(AbstractPipeline<?, E_IN, ?> upstream,
                    StreamShape inputShape,
                    int opFlags) {
            super(upstream, opFlags);
            assert upstream.getOutputShape() == inputShape;
        }
        @Override
        final boolean opIsStateful() {
            return false;
        }

    }


    static class Head<E_IN, E_OUT> extends ReferencePipeline<E_IN, E_OUT>{
        Head(Spliterator<?> source,
             int sourceFlags, boolean parallel) {
            super(source, sourceFlags, parallel);
        }
        @Override
        final Sink<E_IN> opWrapSink(int flags, Sink<E_OUT> sink) {
            throw new UnsupportedOperationException();
        }
        @Override
        final boolean opIsStateful() {
            throw new UnsupportedOperationException();
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public final <R, A> R collect(Collector<? super P_OUT, A, R> collector) {
        A container;
        if (isParallel()
                && (collector.characteristics().contains(Collector.Characteristics.CONCURRENT))
                && (!isOrdered() || collector.characteristics().contains(Collector.Characteristics.UNORDERED))) {
            container = collector.supplier().get();
            BiConsumer<A, ? super P_OUT> accumulator = collector.accumulator();
            forEach(u -> accumulator.accept(container, u));
        }
        else {
            container = evaluate(ReduceOps.makeRef(collector));
        }
        return collector.characteristics().contains(Collector.Characteristics.IDENTITY_FINISH)
                ? (R) container
                : collector.finisher().apply(container);
    }
    @Override
    public void forEach(Consumer<? super P_OUT> action) {
        evaluate(ForEachOps.makeRef(action, false));
    }

    ReferencePipeline(Spliterator<?> source,
                      int sourceFlags, boolean parallel) {
        super(source, sourceFlags, parallel);
    }
    ReferencePipeline(AbstractPipeline<?, P_IN, ?> upstream, int opFlags) {
        super(upstream, opFlags);
    }
}
