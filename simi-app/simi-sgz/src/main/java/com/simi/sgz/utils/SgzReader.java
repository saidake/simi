package com.simi.sgz.utils;

import cn.hutool.setting.yaml.YamlUtil;
import com.simi.sgz.AAAconfig.SgzConstants;
import com.simi.sgz.domain.properties.Coordinate;
import com.simi.sgz.domain.properties.CoordinatesReader;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Field;
import java.util.List;

@UtilityClass
@Slf4j
public class SgzReader {

    public static CoordinatesReader loadCoordinates() throws IllegalAccessException, InstantiationException {
        CoordinatesReader coordinatesReader = YamlUtil.loadByPath(SgzConstants.SGZ_COORDINATES_PATH, CoordinatesReader.class);
        List<Coordinate> coordinates = coordinatesReader.getCoordinates();
        Coordinate coordinate = coordinates.get(0);
        // 736,202 -> 1641,202 -> 736,727 -> 1641,727
        coordinates.add(copyAndIncrement(coordinate,905,0));
        coordinates.add(copyAndIncrement(coordinate,0,525));
        coordinates.add(copyAndIncrement(coordinate,905,525));
        log.debug("coordinatesReader: {}",coordinatesReader);
        return coordinatesReader;
    }
    public static Coordinate copyAndIncrement(Coordinate original, int offsetX, int offsetY) throws IllegalAccessException, InstantiationException {
        // Create a new instance of the original object's class
        // Get all fields of the original class
        Coordinate copy = original.getClass().newInstance();
        Field[] fields = original.getClass().getDeclaredFields();

        for (Field field : fields) {
            field.setAccessible(true); // Allow access to private fields
            // Check if the field is of type int
            if (field.getType() == int[].class) {
                // Increment each element of the int[] field
                int[] originalArray = (int[]) field.get(original);
                if (originalArray != null) {
                    int[] incrementedArray = new int[originalArray.length];
                    incrementedArray[0] = originalArray[0] + offsetX;
                    incrementedArray[1] = originalArray[1] + offsetY;
                    field.set(copy, incrementedArray);
                }
            } else if (field.getType() == int[][].class) {
                // Increment each element of the int[][] field
                int[][] originalArray = (int[][]) field.get(original);
                if (originalArray != null) {
                    int[][] incrementedArray = new int[originalArray.length][];
                    for (int i = 0; i < originalArray.length; i++) {
                        if (originalArray[i] != null) {
                            incrementedArray[i] = new int[originalArray[i].length];
                            incrementedArray[i][0] = originalArray[i][0] + offsetX;
                            incrementedArray[i][1] = originalArray[i][1] + offsetY;
                        }
                    }
                    field.set(copy, incrementedArray);
                }
            } else {
                // Copy the original value if it's not an int, int[], or int[][] field
                field.set(copy, field.get(original));
            }
        }

        return copy;
    }
}
