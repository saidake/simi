package com.simi.trade.repository;

import com.simi.trade.entity.TestPersonEntity;
import com.simi.trade.entity.TestStudentEntity;
import com.simi.common.util.data.RandomUtil;
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
