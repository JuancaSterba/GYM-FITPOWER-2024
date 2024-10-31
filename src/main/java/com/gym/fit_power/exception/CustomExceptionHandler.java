package com.gym.fit_power.exception;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.HashMap;
import java.util.Map;

@RestControllerAdvice
public class CustomExceptionHandler {

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<Map<String, String>> handleValidationExceptions(MethodArgumentNotValidException ex) {
        Map<String, String> errorMap = new HashMap<>();
        errorMap.put("error", "Validation failed");
        errorMap.put("message", ex.getBindingResult().getAllErrors().get(0).getDefaultMessage());
        return ResponseEntity.badRequest().body(errorMap);
    }

}
