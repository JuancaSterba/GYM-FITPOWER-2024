package com.gym.fit_power;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;

@SpringBootApplication(exclude = SecurityAutoConfiguration.class)
public class FitPowerApplication {

	public static void main(String[] args) {
		SpringApplication.run(FitPowerApplication.class, args);
	}

}
