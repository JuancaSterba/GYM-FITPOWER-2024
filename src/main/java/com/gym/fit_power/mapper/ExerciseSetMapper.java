package com.gym.fit_power.mapper;

import com.gym.fit_power.dto.request.ExerciseSetRequestDto;
import com.gym.fit_power.dto.response.ExerciseSetResponseDto;
import com.gym.fit_power.model.Exercise;
import com.gym.fit_power.model.ExerciseSet;
import com.gym.fit_power.model.Routine;
import org.springframework.stereotype.Component;

@Component
public class ExerciseSetMapper {

    public static ExerciseSetResponseDto toDto(ExerciseSet exerciseSet) {
        return ExerciseSetResponseDto.builder()
                .id(exerciseSet.getId())
                .reps(exerciseSet.getReps())
                .sets(exerciseSet.getSets())
                .restInMinutes(exerciseSet.getRestInMinutes())
                .routineId(exerciseSet.getRoutine().getId())
                .exerciseId(exerciseSet.getExercise().getId())
                .build();
    }

    public static ExerciseSet toEntity(ExerciseSetRequestDto exerciseSetRequestDto, Routine routine, Exercise exercise) {

        return ExerciseSet.builder()
                .reps(exerciseSetRequestDto.getReps())
                .sets(exerciseSetRequestDto.getSets())
                .restInMinutes(exerciseSetRequestDto.getRestInMinutes())
                .routine(routine)
                .exercise(exercise)
                .build();
    }
}
