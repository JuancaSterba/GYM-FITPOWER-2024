package com.gym.fit_power.exception;

import static com.gym.fit_power.constant.GymConstants.ERR404;

public class EntityNotFoundException extends RuntimeException{
    public EntityNotFoundException() {
    super(ERR404);
    }

    public EntityNotFoundException(String message, Throwable cause) {
        super(ERR404+": "+message, cause);
    }

    public EntityNotFoundException(String message) {
        super(ERR404 + ": " + message);
    }
}
