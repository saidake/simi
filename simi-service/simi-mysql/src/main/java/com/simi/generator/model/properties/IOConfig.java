package com.simi.generator.model.properties;

import lombok.Data;

@Data
public class IOConfig {
    private Boolean hasDtoTableFolder;
    private Boolean hasServiceTableFolder;
    private Boolean hasControllerTableFolder;
    private Boolean hasMapperResourcesTableFolder;
}
