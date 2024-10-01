package com.simi.common.test.pojo;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.*;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class TestFruit {
    // Basic string field for the fruit name
    private String name;

    // Integer field for the age of the fruit (in days, for example)
    private Integer age;

    // String field for the color of the fruit
    private String color;

    // BigDecimal field for the price of the fruit
    private BigDecimal price;

    // LocalDateTime field for the harvest date of the fruit
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private LocalDateTime harvestDate;

    // Integer field for the quantity available
    private Integer quantityAvailable;

    // Enum to represent fruit types (e.g., citrus, berry, stone fruit)
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private FruitType fruitType;

    public enum FruitType {
        CITRUS, BERRY, STONE_FRUIT, TROPICAL, DRUPE, POME
    }
}
