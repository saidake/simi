package com.saidake.trade.entity.generator;

import com.saidake.common.core.util.data.RandomUtil;
import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.id.IdentifierGenerator;

public class TestStudentIdGenerator implements IdentifierGenerator {
    @Override
    public Object generate(SharedSessionContractImplementor sharedSessionContractImplementor, Object o) throws HibernateException {
        return RandomUtil.getRandomLong(9L,1000000L);
    }
}
