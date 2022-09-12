package com.saidake.citi.repository;
import com.saidake.citi.entity.TestStudent;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.CrudRepository;

public interface TestStudentRepository extends JpaRepository<TestStudent,Long> {
}
