package com.gym.fit_power.exception;


import static com.gym.fit_power.constant.GymConstants.ERR404;
import static com.gym.fit_power.constant.NutritinistConstants.ERROR_UPDATE;

public class EntityUpdateException  extends RuntimeException{
    public EntityUpdateException() {
        super(ERROR_UPDATE);
    }

    public EntityUpdateException(String message) {
        super(ERROR_UPDATE + ": " + message);
    }

    public EntityUpdateException(String message, Throwable cause) {
        super(ERROR_UPDATE+": "+message, cause);
    }
}
