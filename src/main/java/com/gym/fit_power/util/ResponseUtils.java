package com.gym.fit_power.util;

import org.springframework.http.ResponseEntity;

import java.time.LocalDateTime;
import java.util.Map;

public class ResponseUtils {

    public static ResponseEntity<Map<String, String>> createSuccessResponse(String message) {
        return ResponseEntity.ok(Map.of(
                "timestamp", LocalDateTime.now().toString(),
                "message", message
        ));
    }

}
