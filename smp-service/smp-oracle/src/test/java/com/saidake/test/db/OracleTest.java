package com.saidake.test.db;

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
                "jdbc:oracle:thin:@127.0.0.1:1521/ORCL","sdk","sdk");
        Statement s = conn.createStatement();
        String sql="select * from TEST_STUDENT";
        ResultSet result=s.executeQuery(sql);
        System.out.println(result);
    }
}
