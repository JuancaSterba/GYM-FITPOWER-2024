package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.TrainerRequestDto;
import com.gym.fit_power.dto.response.TrainerResponseDto;
import com.gym.fit_power.model.Trainer;
import jakarta.persistence.EntityNotFoundException;

import java.util.List;

public interface TrainerService {
    Trainer toEntity(TrainerRequestDto dto);
    TrainerResponseDto toDto(Trainer trainer);

    List<TrainerResponseDto> toDtos(List<Trainer> trainers);
    TrainerResponseDto findByDni(String dni) throws EntityNotFoundException;
    TrainerResponseDto save(TrainerRequestDto trainerRequestDto);
    TrainerResponseDto update(String dni, TrainerRequestDto trainerRequestDto) throws EntityNotFoundException;
    void delete(String dni) throws EntityNotFoundException;
}
