//package com.simi.citi.AAAconfig;
//
//import com.simi.citi.entity.CronEntity;
//import com.simi.citi.repository.CronRepository;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.context.annotation.Configuration;
//import org.springframework.scheduling.annotation.Async;
//import org.springframework.scheduling.annotation.EnableAsync;
//import org.springframework.scheduling.annotation.EnableScheduling;
//import org.springframework.scheduling.annotation.SchedulingConfigurer;
//import org.springframework.scheduling.config.ScheduledTaskRegistrar;
//import org.springframework.scheduling.support.CronTrigger;
//
//
//@Configuration
//@EnableScheduling
//@Slf4j
//public class DynamicScheduleTask implements SchedulingConfigurer {
//    @Autowired
//    CronRepository cornRepository;
//
//    @Override
//    public void configureTasks(ScheduledTaskRegistrar taskRegistrar) {
//        taskRegistrar.addTriggerTask(this::execute, triggerContext -> {
//            String corn=null;
//            System.out.println(cornRepository.findAll());
//            CronEntity resultEntity = cornRepository.findFirstByCronName("CLEAR_SYS_LOG");
//            if(resultEntity==null) System.out.println("cannnot find cron");
//            if(resultEntity!=null)corn =resultEntity.getCron();
//            System.out.println("cron: "+corn);
//            CronTrigger cronTrigger = new CronTrigger("0/6 * * * * ?");
//            return cronTrigger.nextExecutionTime(triggerContext);
//        });
//    }
//
//    public void execute() {
//        log.info("执行任务");
//        try {
//            Thread.sleep(3000);
//        } catch (InterruptedException e) {
//            e.printStackTrace();
//        }
//        log.info("success");
//    }
//}
