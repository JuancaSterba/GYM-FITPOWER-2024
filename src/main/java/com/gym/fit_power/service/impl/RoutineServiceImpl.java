package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.request.RoutineRequestDto;
import com.gym.fit_power.dto.response.RoutineResponseDto;
import com.gym.fit_power.service.RoutineService;
import org.springframework.stereotype.Service;

@Service
public class RoutineServiceImpl implements RoutineService {

    @Override
    public RoutineResponseDto save(RoutineRequestDto routineRequestDto, String trainerCuit, String clientCuit) {
        return null;
    }

    @Override
    public RoutineResponseDto findClientActiveRoutine(String clientCuit) {
        return null;
    }

}
