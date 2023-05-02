package com.saidake.trade.repository;

import com.saidake.trade.entity.TestPersonEntity;
import com.saidake.trade.entity.TestStudentEntity;
import com.saidake.common.core.util.data.RandomUtil;
import org.springframework.stereotype.Repository;

import jakarta.annotation.Resource;
import jakarta.transaction.Transactional;

@Repository
public class TestStudentSaveLogRepository {

    @Resource
    private TestStudentRepository testStudentRepository;

    @Resource
    TestPersonRepository testPersonRepository;

    @Transactional
    public void save(TestStudentEntity testStudentEntity){
        System.out.println("testStudentEntity: "+testStudentEntity);
        testStudentRepository.save(testStudentEntity);
        TestPersonEntity testPersonEntity=new TestPersonEntity();
        testPersonEntity.setPerId(RandomUtil.getRandomLong(100L,300L));
        testPersonEntity.setAge(3L);
        testPersonEntity.setGender(0);
        testPersonEntity.setName("logName"+testStudentEntity.getClassName());
        System.out.println("testPersonEntity: "+testPersonEntity);
        testPersonRepository.save(testPersonEntity);
    }

}
