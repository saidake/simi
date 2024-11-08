package com.simi.common.log.entity;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode
public class SysLog {

    private static final long serialVersionUID = 1L;

    @Schema(description = "Log ID")
    private Long id;

    @Schema(description = "Log Type")
    private String type;

    @Schema(description = "Log Title")
    private String title;

    @Schema(description = "Operation IP Address")
    private String remoteAddr;

    @Schema(description = "User Agent")
    private String userAgent;

    @Schema(description = "Request URI")
    private String requestUri;

    @Schema(description = "Operation Method")
    private String method;

    @Schema(description = "Data")
    private String params;

    @Schema(description = "Method Execution Time")
    private Long time;

    @Schema(description = "Exception Information")
    private String exception;

    @Schema(description = "Service Identifier")
    private String serviceId;

    @Schema(description = "Delete Flag")
    private String delFlag;

}
