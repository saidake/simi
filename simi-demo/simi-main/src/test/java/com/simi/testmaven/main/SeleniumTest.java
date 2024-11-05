package com.simi.testmaven.main;


import com.simi.testmaven.main.AAAConfig.WebDriverConfig;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
@Import(WebDriverConfig.class)
@SpringBootTest
public class SeleniumTest {
    @Autowired
    private WebDriver driver;
    @Test
    public void testSelenium() throws InterruptedException {
        driver.get("https://www.google.com");
        driver.findElement(By.name("q")).sendKeys("Selenium");
        driver.findElement(By.name("btnK")).click();
        Thread.sleep(30000);
        driver.quit();
    }
}
