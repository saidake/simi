package com.simi.test.db;

import lombok.extern.slf4j.Slf4j;
import org.hibernate.SessionFactory;
import org.hibernate.boot.SessionFactoryBuilder;

import java.sql.*;
import java.util.Arrays;
import java.util.List;

@Slf4j
public class OracleTest {
    public static void main(String[] args) throws ClassNotFoundException, SQLException {
        Class.forName("oracle.jdbc.OracleDriver");
        Connection conn = DriverManager.getConnection(
                "jdbc:oracle:thin:@127.0.0.1:1521:xe","c##simi","simi");
        Statement s = conn.createStatement();
        String sql="select 1 from dual";
        ResultSet result=s.executeQuery(sql);
        System.out.println(result);
    }
}
