package com.gym.fit_power.exception;

import jakarta.persistence.EntityNotFoundException;

public class RoutineNotFoundException extends EntityNotFoundException {
    public RoutineNotFoundException(String message) {
        super(message);
    }
}
