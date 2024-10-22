package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.TrainerRequestDto;
import com.gym.fit_power.dto.response.TrainerResponseDto;
import com.gym.fit_power.exception.DuplicatedTrainerException;
import com.gym.fit_power.exception.TrainerNotFoundException;
import com.gym.fit_power.exception.TrainerUpdateException;

import java.util.List;

public interface TrainerService {
    List<TrainerResponseDto> findAll();
    TrainerResponseDto findByDni(String dni) throws TrainerNotFoundException;
    TrainerResponseDto save(TrainerRequestDto trainerRequestDto) throws DuplicatedTrainerException;
    TrainerResponseDto update(String dni, TrainerRequestDto trainerRequestDto) throws TrainerUpdateException;
    void delete(String dni) throws TrainerNotFoundException;
}
