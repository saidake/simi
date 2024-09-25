package com.simi.webflux.controller;


import com.simi.common.test.DataFactory;
import com.simi.common.test.pojo.Person;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/test")
public class TestController {
    @GetMapping("/mono/{name}")
    public Mono<Integer> testGetMono(Person per){
        return Mono.just(per.getAge());
    }
    @GetMapping("/flux/{name}")
    public Flux<Person> testGetFlux(Person per){
        return Flux.fromIterable(DataFactory.getPersonList());
    }
}
