package com.simi.getdatasimultaneously;

import java.util.concurrent.*;

public class GetDataSimultaneously {
    public static void main(String[] args) {
        AService aService= new AService();
        BService bService= new BService();
        CService cService= new CService();
        ThreadPoolExecutor threadPoolExecutor =
                new ThreadPoolExecutor(3, 3, 30, TimeUnit.SECONDS, new ArrayBlockingQueue<>(1));
        CompletableFuture<Integer> aCompletableFuture = CompletableFuture.supplyAsync(aService::get);
        CompletableFuture<Integer> bCompletableFuture =CompletableFuture.supplyAsync(bService::get);
        CompletableFuture<Integer> cCompletableFuture =CompletableFuture.supplyAsync(cService::get);
        CompletableFuture<Void> voidCompletableFuture = CompletableFuture.allOf(aCompletableFuture, bCompletableFuture, cCompletableFuture);
        CompletableFuture<Integer> integerCompletableFuture = voidCompletableFuture.thenApply(val -> aCompletableFuture.join() + bCompletableFuture.join() + cCompletableFuture.join());
        Integer sum=integerCompletableFuture.join();
        System.out.println(sum);
    }
}
