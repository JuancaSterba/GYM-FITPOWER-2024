package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.ExerciseSetRequestDto;
import com.gym.fit_power.dto.response.ExerciseSetResponseDto;
import com.gym.fit_power.exception.EntitySavedException;
import com.gym.fit_power.exception.ExerciseSetNotFoundException;

import java.util.List;

public interface ExerciseSetService {
    List<ExerciseSetResponseDto> findAllByRoutine(Long routineId);
    void save(Long routineId, Long exerciseId, ExerciseSetRequestDto exerciseSetRequestDto) throws EntitySavedException;
    void update(Long routineId, Long exerciseId, ExerciseSetRequestDto exerciseSetRequestDto) throws ExerciseSetNotFoundException;
    void delete(Long routineId, Long exerciseId, ExerciseSetRequestDto exerciseSetRequestDto) throws ExerciseSetNotFoundException;
}
