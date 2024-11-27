package com.gym.fit_power.mapper;

import com.gym.fit_power.dto.request.ExerciseRequestDto;
import com.gym.fit_power.dto.response.ExerciseResponseDto;
import com.gym.fit_power.model.Exercise;
import org.springframework.stereotype.Component;

@Component
public class ExerciseMapper {

    public ExerciseResponseDto toDto(Exercise exercise) {
        return ExerciseResponseDto.builder()
                .id(exercise.getId())
                .name(exercise.getName())
                .description(exercise.getDescription())
                .muscleGroup(exercise.getMuscleGroup())
                .enabled(exercise.isEnabled())
                .build();
    }

    public static Exercise toEntity(ExerciseRequestDto exerciseRequestDto) {
        return Exercise.builder()
                .name(exerciseRequestDto.getName())
                .description(exerciseRequestDto.getDescription())
                .muscleGroup(exerciseRequestDto.getMuscleGroup())
                .build();
    }
}
