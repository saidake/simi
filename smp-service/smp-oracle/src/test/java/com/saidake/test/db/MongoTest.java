package com.saidake.test.db;

import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import org.bson.Document;

public class MongoTest {
    public static void main(String[] args) {
        MongoClient mongoClient = MongoClients.create("mongodb://smp:smp@127.0.0.1:27017");
        MongoDatabase smp = mongoClient.getDatabase("smp");
        MongoCollection<Document> test = smp.getCollection("test");
        System.out.println(test.countDocuments());
        System.out.println("Connect to database successfully");
    }
}
