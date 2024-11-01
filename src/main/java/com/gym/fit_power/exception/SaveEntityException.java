package com.gym.fit_power.exception;

import static com.gym.fit_power.constant.NutritinistConstants.SUCCESSFUL;

public class SaveEntityException extends RuntimeException {

    public SaveEntityException() {
        super(SUCCESSFUL);
    }

    public SaveEntityException(String message, Throwable cause) {
        super(SUCCESSFUL + ": " + message, cause);
    }

    public SaveEntityException(String message) {
        super(SUCCESSFUL + ": " + message);
    }

}
