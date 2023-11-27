package com.simi.trade.repository;
import com.simi.trade.entity.TestStudentEntity;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;


public interface TestStudentRepository extends JpaRepository<TestStudentEntity, Long> {

    TestStudentEntity findFirstByStuId(Long stuId);

    TestStudentEntity findFirstByStuIdOrRole(Long stuId, String role);
    @Query("select ts from TestStudentEntity ts where ts.stuId=:stuId or ts.role=:role")
    TestStudentEntity findFirstByStuIdOrRoleCus(Long stuId, String role);

    List<TestStudentEntity> findByStuIdOrRole(Long stuId, String role, Pageable pageable);
    @Query("select ts from TestStudentEntity ts where ts.stuId=:stuId or ts.role=:role")
    List<TestStudentEntity> findByStuIdOrRoleCus(Long stuId, String role, Pageable pageable);
    @Query(value = "select * from TEST_STUDENT  where STU_ID=:stuId or ROLE=:role limit 0,1",nativeQuery = true)
    TestStudentEntity findFirstByStuIdOrRoleNative(Long stuId, String role);


    @Modifying
    @Query("delete from TestStudentEntity t where t.perId =:perId")
    Integer deletByPerId(Long perId);

}
