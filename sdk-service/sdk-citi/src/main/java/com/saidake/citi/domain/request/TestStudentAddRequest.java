package com.saidake.citi.domain.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@Schema(name = "TEST_STUDENT_COMMENT add model")
public class TestStudentAddRequest {

    private Long stuId;

    private Long perId;

    private String className;

    private String role;
}
