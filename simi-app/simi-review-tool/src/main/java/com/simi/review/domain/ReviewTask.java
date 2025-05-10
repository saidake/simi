package com.simi.review.domain;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ReviewTask {
    // Required Properties
    private String file;

    // Optional Properties
    private String matchPattern;
    private String removalPattern;
    private Integer number;
}
