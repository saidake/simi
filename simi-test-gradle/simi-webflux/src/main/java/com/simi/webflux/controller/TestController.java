package com.simi.webflux.controller;


import com.simi.common.test.DataFactory;
import com.simi.common.test.pojo.TestFruit;
import com.simi.webflux.service.TestPersonService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/test")
public class TestController {
    @GetMapping("/mono/{name}")
    public Mono<Integer> testGetMono(TestFruit fu){
        return Mono.just(fu.getAge());
    }
    @GetMapping("/flux/{name}")
    public Flux<TestFruit> testGetFlux(TestFruit per){
        return Flux.fromIterable(DataFactory.getTestFruitList());
    }

}
