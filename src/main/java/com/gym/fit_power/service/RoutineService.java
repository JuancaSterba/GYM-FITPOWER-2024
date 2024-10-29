package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.RoutineRequestDto;
import com.gym.fit_power.dto.response.RoutineResponseDto;

import java.util.List;

public interface RoutineService {
    List<RoutineResponseDto> findAllByTrainer(String dni);
    List<RoutineResponseDto> findAllByCustomer(String dni);
    RoutineResponseDto findByCode(String code);
    RoutineResponseDto findEnabledByCustomer(String dni);

    RoutineResponseDto save(RoutineRequestDto routineRequestDto, String trainerDni, String customerDni);
    RoutineResponseDto update(String code, RoutineRequestDto routineRequestDto);
    void delete(String code);
}
