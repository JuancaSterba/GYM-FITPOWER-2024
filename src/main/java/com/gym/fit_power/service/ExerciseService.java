package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.ExerciseRequestDto;
import com.gym.fit_power.dto.response.ExerciseResponseDto;
import com.gym.fit_power.exception.DuplicatedExerciseException;
import com.gym.fit_power.exception.ExerciseNotFoundException;
import com.gym.fit_power.exception.ExerciseUpdateException;

import java.util.List;
import java.util.Optional;

public interface ExerciseService {
    List<ExerciseResponseDto> findAll();
    ExerciseResponseDto findByName(String name) throws ExerciseNotFoundException;
    ExerciseResponseDto findByMuscleGroup(String muscleGroup) throws ExerciseNotFoundException;
    ExerciseResponseDto save(ExerciseRequestDto exerciseRequestDto) throws DuplicatedExerciseException;
    ExerciseResponseDto update(String name, ExerciseRequestDto exerciseRequestDto) throws ExerciseUpdateException;
    void delete(String name) throws ExerciseNotFoundException;
}
