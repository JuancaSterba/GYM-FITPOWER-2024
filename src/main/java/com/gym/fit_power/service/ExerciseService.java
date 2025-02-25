package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.ExerciseRequestDto;
import com.gym.fit_power.dto.response.ExerciseResponseDto;
import com.gym.fit_power.exception.EntityNotFoundException;
import com.gym.fit_power.exception.EntitySaveException;
import com.gym.fit_power.exception.EntityUpdateException;

import java.util.List;

public interface ExerciseService {
    List<ExerciseResponseDto> findAll();
    ExerciseResponseDto findByName(String name) throws EntityNotFoundException;
    ExerciseResponseDto findByMuscleGroup(String muscleGroup) throws EntityNotFoundException;
    ExerciseResponseDto save(ExerciseRequestDto exerciseRequestDto) throws EntitySaveException;
    ExerciseResponseDto update(String name, ExerciseRequestDto exerciseRequestDto) throws EntityUpdateException;
    void delete(String name) throws EntityNotFoundException;
}
