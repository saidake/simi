package com.saidake.test.data;

import lombok.*;
import lombok.extern.jackson.Jacksonized;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class Toy {
    public String name;
    public String type;
}
