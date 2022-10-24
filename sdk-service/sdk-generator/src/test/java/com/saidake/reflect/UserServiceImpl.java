package com.saidake.reflect;

public class UserServiceImpl implements UserService {

    @Override
    public String query() {
        System.out.println("UserServiceImpl query");
        return "UserServiceImpl query";
    }
}
