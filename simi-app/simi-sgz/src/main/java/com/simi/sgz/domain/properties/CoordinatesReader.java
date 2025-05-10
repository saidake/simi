package com.simi.sgz.domain.properties;

// Main class to read the coordinates
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class CoordinatesReader {
    private List<Coordinate> coordinates;

}