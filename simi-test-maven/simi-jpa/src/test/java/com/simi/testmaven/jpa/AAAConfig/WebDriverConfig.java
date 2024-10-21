package com.simi.testmaven.jpa.AAAConfig;

import io.github.bonigarcia.wdm.WebDriverManager;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;

/**
 * An isolated configuration class for testing selenium-java that
 * performs browser-based JUnit tests.
 */
@Configuration
public class WebDriverConfig {
    @Bean
    public WebDriver chromeDriver() {
        // Set up ChromeDriver using WebDriverManager
        WebDriverManager.chromedriver().setup();
        ChromeOptions options = new ChromeOptions();
        options.addArguments("--no-sandbox");
        options.addArguments("--disable-dev-shm-usage");
        //options.addArguments("--headless"); // Optional: run in headless mode if needed
        options.addArguments("--remote-debugging-port=9222");
        options.addArguments("--disable-web-security"); // Disable web security to avoid CORS issues
        options.addArguments("--remote-allow-origins=*");
        // Set timeouts
        options.setImplicitWaitTimeout(Duration.ofSeconds(10)); // Adjust as necessary
        options.setPageLoadTimeout(Duration.ofSeconds(30)); // Adjust as necessary

        return new ChromeDriver(options);
    }
}
