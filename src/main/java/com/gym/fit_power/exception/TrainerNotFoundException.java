package com.gym.fit_power.exception;

import jakarta.persistence.EntityNotFoundException;

public class TrainerNotFoundException extends EntityNotFoundException {

    public TrainerNotFoundException() {
        super();
    }

    public TrainerNotFoundException(String message) {
        super(message);
    }

}
