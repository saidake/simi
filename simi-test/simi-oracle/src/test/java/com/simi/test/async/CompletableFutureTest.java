package com.simi.test.async;

import com.simi.support.data.DataFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

public class CompletableFutureTest {
    public static void main(String[] args) {

        List<CompletableFuture<Integer>> futureList=new ArrayList<>();
        futureList.add(CompletableFuture.supplyAsync(()->{
            return null;
        }));
        futureList.add(CompletableFuture.supplyAsync(()->{
            return 1;
        }));
        futureList.add(CompletableFuture.supplyAsync(()->{
            System.out.println("ddd");
            return 2;
        }));
        CompletableFuture.allOf(futureList.toArray(new CompletableFuture[futureList.size()])).join();
        List<Integer> collect = futureList.stream().map(CompletableFuture::join).collect(Collectors.toList());
        System.out.println(collect);
    }
}
