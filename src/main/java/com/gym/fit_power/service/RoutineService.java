package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.RoutineRequestDto;
import com.gym.fit_power.dto.response.RoutineResponseDto;

public interface RoutineService {
    RoutineResponseDto save(RoutineRequestDto routineRequestDto, String trainerCuit, String clientCuit);
    RoutineResponseDto findClientActiveRoutine(String clientCuit);
    void disableActiveRoutine(Long routineId);
}
