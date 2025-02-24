package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.TrainerRequestDto;
import com.gym.fit_power.dto.response.TrainerResponseDto;
import com.gym.fit_power.exception.EntityDuplicatedException;
import com.gym.fit_power.exception.EntityNotFoundException;
import com.gym.fit_power.exception.EntityUpdateException;

import java.util.List;

public interface TrainerService {
    List<TrainerResponseDto> findAll();
    TrainerResponseDto findByCuit(String cuit) throws EntityNotFoundException;
    TrainerResponseDto save(TrainerRequestDto trainerRequestDto) throws EntityDuplicatedException;
    TrainerResponseDto update(String cuit, TrainerRequestDto trainerRequestDto) throws EntityUpdateException;
    void delete(String cuit) throws EntityNotFoundException;
}
