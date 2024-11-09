package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.request.ExerciseRequestDto;
import com.gym.fit_power.dto.response.ExerciseResponseDto;
import com.gym.fit_power.exception.DuplicatedExerciseException;
import com.gym.fit_power.exception.ExerciseNotFoundException;
import com.gym.fit_power.exception.ExerciseUpdateException;
import com.gym.fit_power.mapper.ExerciseMapper;
import com.gym.fit_power.model.Exercise;
import com.gym.fit_power.repository.ExerciseRepository;
import com.gym.fit_power.service.ExerciseService;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

public class ExerciseServiceImpl implements ExerciseService {

    private final ExerciseRepository exerciseRepository;
    private final ExerciseMapper exerciseMapper;

    public ExerciseServiceImpl(ExerciseRepository exerciseRepository, ExerciseMapper exerciseMapper) {
        this.exerciseRepository = exerciseRepository;
        this.exerciseMapper = exerciseMapper;
    }

    @Override
    @Transactional(readOnly = true)
    public List<ExerciseResponseDto> findAll() {
        return exerciseRepository
                .findAll()
                .stream()
                .map(exerciseMapper::toDto)
                .toList();
    }

    @Override
    @Transactional(readOnly = true)
    public ExerciseResponseDto findByName(String name) throws ExerciseNotFoundException {
        Optional<Exercise> exercise = exerciseRepository.findByName(name);
        return exercise.map(exerciseMapper::toDto).orElseThrow(() -> new ExerciseNotFoundException("Exercise with name " + name + " not found."));
    }

    @Override
    @Transactional(readOnly = true)
    public ExerciseResponseDto findByMuscleGroup(String muscleGroup) throws ExerciseNotFoundException {
        Optional<Exercise> exercise = exerciseRepository.findByMuscleGroup(muscleGroup);
        return exercise.map(exerciseMapper::toDto).orElseThrow(() -> new ExerciseNotFoundException("Exercise with muscle group " + muscleGroup + " not found."));
    }

    @Override
    @Transactional
    public ExerciseResponseDto save(ExerciseRequestDto exerciseRequestDto) throws DuplicatedExerciseException {
        exerciseRepository.findByName(exerciseRequestDto.getName())
                .ifPresent(exercise -> {
                    throw new DuplicatedExerciseException("Exercise with name " + exerciseRequestDto.getName() + " already exists.");
                });
        Exercise newExercise = exerciseMapper.toEntity(exerciseRequestDto);
        newExercise = exerciseRepository.save(newExercise);
        return exerciseMapper.toDto(newExercise);
    }

    @Override
    @Transactional
    public ExerciseResponseDto update(String name, ExerciseRequestDto exerciseRequestDto) throws ExerciseUpdateException {
        Exercise exercise = exerciseRepository.findByName(name)
                .orElseThrow(() -> new ExerciseUpdateException("Exercise with name " + name + " not found."));
        Exercise updatedExercise = exerciseMapper.toEntity(exerciseRequestDto);
        updatedExercise.setId(exercise.getId());
        updatedExercise = exerciseRepository.save(updatedExercise);
        return exerciseMapper.toDto(updatedExercise);
    }

    @Override
    @Transactional
    public void delete(String name) throws ExerciseNotFoundException {
        Exercise exercise = exerciseRepository.findByName(name)
                .orElseThrow(() -> new ExerciseNotFoundException("Exercise with name " + name + " not found."));
        exercise.setEnabled(false);
        exerciseRepository.save(exercise);
    }
}
