package com.simi.common.log.entity;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;


@Data
@EqualsAndHashCode
public class SysLog  {

    private static final long serialVersionUID = 1L;

    @Schema(description = "日志编号")
    private Long id;

    @Schema(description = "日志类型")
    private String type;

    @Schema(description = "日志标题")
    private String title;

    @Schema(description = "操作ip地址")
    private String remoteAddr;

    @Schema(description = "用户代理")
    private String userAgent;

    @Schema(description = "请求uri")
    private String requestUri;

    @Schema(description = "操作方式")
    private String method;

    @Schema(description = "数据")
    private String params;

    @Schema(description = "方法执行时间")
    private Long time;

    @Schema(description = "异常信息")
    private String exception;

    @Schema(description = "应用标识")
    private String serviceId;

    @Schema(description = "删除标记")
    private String delFlag;

}
