package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.ExerciseSetRequestDto;
import com.gym.fit_power.dto.response.ExerciseSetResponseDto;
import com.gym.fit_power.exception.EntityNotFoundException;
import com.gym.fit_power.exception.EntitySaveException;

import java.util.List;

public interface ExerciseSetService {
    List<ExerciseSetResponseDto> findAllByRoutine(Long routineId);
    void save(Long routineId, Long exerciseId, ExerciseSetRequestDto exerciseSetRequestDto) throws EntitySaveException;
    void update(Long routineId, Long exerciseId, ExerciseSetRequestDto exerciseSetRequestDto) throws EntityNotFoundException;
    void delete(Long routineId, Long exerciseId, ExerciseSetRequestDto exerciseSetRequestDto) throws EntityNotFoundException;
}
