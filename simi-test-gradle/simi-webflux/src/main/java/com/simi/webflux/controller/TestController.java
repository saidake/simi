package com.simi.webflux.controller;


import com.simi.common.test.DataFactory;
import com.simi.common.test.pojo.Person;
import com.simi.webflux.service.PersonService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/test")
public class TestController {
    @Autowired
    PersonService personService;
    @GetMapping("/mono/{name}")
    public Mono<Integer> testGetMono(Person per){
        return Mono.just(per.getAge());
    }
    @GetMapping("/flux/{name}")
    public Flux<Person> testGetFlux(Person per){
        return Flux.fromIterable(DataFactory.getPersonList());
    }


    @PostMapping
    public void addPerson(@RequestBody com.simi.webflux.domain.Person person) {
        personService.addPerson(person);
    }

    @GetMapping("/{personId}")
    public com.simi.webflux.domain.Person getPerson(@PathVariable String personId) {
        return personService.getPersonById(personId);
    }
}
