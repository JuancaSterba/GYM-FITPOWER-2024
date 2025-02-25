package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.TrainerRequestDto;
import com.gym.fit_power.dto.response.TrainerResponseDto;
import com.gym.fit_power.exception.EntityNotFoundException;
import com.gym.fit_power.exception.EntitySaveException;

import java.util.List;

public interface TrainerService {
    List<TrainerResponseDto> findAll();
    TrainerResponseDto findByCuit(String cuit) throws EntityNotFoundException;
    TrainerResponseDto save(TrainerRequestDto trainerRequestDto) throws EntitySaveException;
    TrainerResponseDto update(String cuit, TrainerRequestDto trainerRequestDto) throws EntitySaveException;
    void delete(String cuit) throws EntityNotFoundException;
}
