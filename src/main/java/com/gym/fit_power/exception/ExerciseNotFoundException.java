package com.gym.fit_power.exception;

public class ExerciseNotFoundException extends RuntimeException {

    public ExerciseNotFoundException() {
        super();
    }
    public ExerciseNotFoundException(String message) {
        super(message);
    }

}
