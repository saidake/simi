package com.simi.generator.mybatis.mapper;
import com.simi.generator.mybatis.entity.ColumnEntity;
import com.simi.generator.mybatis.entity.TableEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface DatabaseMapper {
    List<TableEntity> queryTableList(String tableName);
    List<ColumnEntity> queryColumnList(@Param("tableName")  String tableName,@Param("passFields")  List<String> passFields);
    List<String> queryDistinctTableDataType(@Param("tableName")  String tableName,@Param("passFields")  List<String> passFields);
}
