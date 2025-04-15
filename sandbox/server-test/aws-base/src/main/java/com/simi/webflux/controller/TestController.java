package com.simi.webflux.controller;

import com.simi.common.test.DataFactory;
import com.simi.common.test.domain.TestFruit;
import io.micrometer.tracing.annotation.NewSpan;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/test")
public class TestController {
    @GetMapping("/mono/{name}")
    @NewSpan("create new user and group (@NewSpan)")
    public Mono<Integer> testGetMono(TestFruit fu){
        return Mono.just(fu.getAge());
    }
    @GetMapping("/flux/{name}")
    public Flux<TestFruit> testGetFlux(TestFruit per){
        return Flux.fromIterable(DataFactory.getTestFruitList());
    }

}
