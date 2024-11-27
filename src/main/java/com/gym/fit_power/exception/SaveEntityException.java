package com.gym.fit_power.exception;

import static com.gym.fit_power.constant.NutritinistConstants.ERROR_SAVE;


public class SaveEntityException extends RuntimeException {

    public SaveEntityException() {
        super(ERROR_SAVE);
    }

    public SaveEntityException(String message, Throwable cause) {
        super(ERROR_SAVE + ": " + message, cause);
    }

    public SaveEntityException(String message) {
        super(ERROR_SAVE + ": " + message);
    }

}
