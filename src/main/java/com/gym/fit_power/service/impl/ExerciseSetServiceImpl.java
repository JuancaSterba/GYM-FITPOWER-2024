package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.request.ExerciseRequestDto;
import com.gym.fit_power.dto.request.ExerciseSetRequestDto;
import com.gym.fit_power.dto.response.ExerciseSetResponseDto;
import com.gym.fit_power.exception.ExerciseSetException;
import com.gym.fit_power.exception.ExerciseSetNotFoundException;
import com.gym.fit_power.mapper.ExerciseSetMapper;
import com.gym.fit_power.model.Exercise;
import com.gym.fit_power.model.ExerciseSet;
import com.gym.fit_power.model.Routine;
import com.gym.fit_power.repository.ExerciseRepository;
import com.gym.fit_power.repository.ExerciseSetRepository;
import com.gym.fit_power.repository.RoutineRepository;
import com.gym.fit_power.service.ExerciseSetService;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

public class ExerciseSetServiceImpl implements ExerciseSetService {

    private final ExerciseSetRepository exerciseSetRepository;
    private final RoutineRepository routineRepository;
    private final ExerciseRepository exerciseRepository;


    public ExerciseSetServiceImpl(ExerciseSetRepository exerciseSetRepository, RoutineRepository routineRepository, ExerciseRepository exerciseRepository, ExerciseSetMapper exerciseSetMapper) {
        this.exerciseSetRepository = exerciseSetRepository;
        this.routineRepository = routineRepository;
        this.exerciseRepository = exerciseRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public List<ExerciseSetResponseDto> findAllByRoutine(Long routineId) {
        return exerciseSetRepository.findAllByRoutineId(routineId)
                .stream()
                .map(ExerciseSetMapper::toDto)
                .toList();
    }

    @Override
    @Transactional
    public void save(Long routineId, Long exerciseId, ExerciseSetRequestDto exerciseSetRequestDto) throws ExerciseSetException {
        Routine routine = routineRepository.findById(routineId).orElseThrow(() -> new ExerciseSetException("Routine not found"));
        Exercise exercise = exerciseRepository.findById(exerciseId).orElseThrow(() -> new ExerciseSetException("Exercise not found"));
        exerciseSetRepository.save(ExerciseSetMapper.toEntity(exerciseSetRequestDto, routine, exercise));
    }

    @Override
    @Transactional
    public void update(Long routineId, Long exerciseId, ExerciseSetRequestDto exerciseSetRequestDto) throws ExerciseSetNotFoundException {
        ExerciseSet exerciseSet = exerciseSetRepository.findByRoutineIdAndExerciseId(routineId, exerciseId)
                .orElseThrow(() -> new ExerciseSetNotFoundException("ExerciseSet not found"));
        ExerciseSet updatedExerciseSet = ExerciseSetMapper.toEntity(exerciseSetRequestDto, exerciseSet.getRoutine(), exerciseSet.getExercise());
        updatedExerciseSet.setId(exerciseSet.getId());
        exerciseSetRepository.save(updatedExerciseSet);
    }

    @Override
    @Transactional
    public void delete(Long routineId, Long exerciseId, ExerciseSetRequestDto exerciseSetRequestDto) throws ExerciseSetNotFoundException {
        ExerciseSet exerciseSet = exerciseSetRepository.findByRoutineIdAndExerciseId(routineId, exerciseId)
                .orElseThrow(() -> new ExerciseSetNotFoundException("ExerciseSet not found"));
        exerciseSetRepository.delete(exerciseSet);
    }

}
