package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.request.ExerciseRequestDto;
import com.gym.fit_power.dto.response.ExerciseResponseDto;
import com.gym.fit_power.exception.EntityDuplicatedException;
import com.gym.fit_power.exception.EntityUpdateException;
import com.gym.fit_power.exception.ExerciseNotFoundException;
import com.gym.fit_power.model.Exercise;
import com.gym.fit_power.repository.ExerciseRepository;
import com.gym.fit_power.service.ExerciseService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Service
public class ExerciseServiceImpl implements ExerciseService {

    private final ExerciseRepository exerciseRepository;

    public ExerciseServiceImpl(ExerciseRepository exerciseRepository) {
        this.exerciseRepository = exerciseRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public List<ExerciseResponseDto> findAll() {
        return exerciseRepository
                .findAll()
                .stream()
                .map(this::toDto)
                .toList();
    }

    @Override
    @Transactional(readOnly = true)
    public ExerciseResponseDto findByName(String name) throws ExerciseNotFoundException {
        Optional<Exercise> exercise = exerciseRepository.findByName(name);
        return exercise.map(this::toDto).orElseThrow(() -> new ExerciseNotFoundException("Exercise with name " + name + " not found."));
    }

    @Override
    @Transactional(readOnly = true)
    public ExerciseResponseDto findByMuscleGroup(String muscleGroup) throws ExerciseNotFoundException {
        Optional<Exercise> exercise = exerciseRepository.findByMuscleGroup(muscleGroup);
        return exercise.map(this::toDto).orElseThrow(() -> new ExerciseNotFoundException("Exercise with muscle group " + muscleGroup + " not found."));
    }

    @Override
    @Transactional
    public ExerciseResponseDto save(ExerciseRequestDto exerciseRequestDto) throws EntityDuplicatedException {
        exerciseRepository.findByName(exerciseRequestDto.getName())
                .ifPresent(exercise -> {
                    throw new EntityDuplicatedException("Exercise with name " + exerciseRequestDto.getName() + " already exists.");
                });
        Exercise newExercise = this.toEntity(exerciseRequestDto);
        newExercise = exerciseRepository.save(newExercise);
        return this.toDto(newExercise);
    }

    @Override
    @Transactional
    public ExerciseResponseDto update(String name, ExerciseRequestDto exerciseRequestDto) throws EntityUpdateException {
        Exercise exercise = exerciseRepository.findByName(name)
                .orElseThrow(() -> new EntityUpdateException("Exercise with name " + name + " not found."));
        Exercise updatedExercise = this.toEntity(exerciseRequestDto);
        updatedExercise.setId(exercise.getId());
        updatedExercise = exerciseRepository.save(updatedExercise);
        return this.toDto(updatedExercise);
    }

    @Override
    @Transactional
    public void delete(String name) throws ExerciseNotFoundException {
        Exercise exercise = exerciseRepository.findByName(name)
                .orElseThrow(() -> new ExerciseNotFoundException("Exercise with name " + name + " not found."));
        exercise.setEnabled(false);
        exerciseRepository.save(exercise);
    }

    private ExerciseResponseDto toDto(Exercise exercise) {
        return ExerciseResponseDto.builder()
                .id(exercise.getId())
                .name(exercise.getName())
                .description(exercise.getDescription())
                .muscleGroup(exercise.getMuscleGroup())
                .enabled(exercise.isEnabled())
                .build();
    }

    private Exercise toEntity(ExerciseRequestDto exerciseRequestDto) {
        return Exercise.builder()
                .name(exerciseRequestDto.getName())
                .description(exerciseRequestDto.getDescription())
                .muscleGroup(exerciseRequestDto.getMuscleGroup())
                .build();
    }

}
