package com.gym.fit_power.mapper;

import com.gym.fit_power.dto.request.RoutineRequestDto;
import com.gym.fit_power.dto.response.RoutineResponseDto;
import com.gym.fit_power.model.Routine;
import org.springframework.stereotype.Component;

@Component
public class RoutineMapper {

    public static RoutineResponseDto toDto(Routine routine) {
        return RoutineResponseDto.builder()
                .id(routine.getId())
                .goals(routine.getGoals())
                .createdAt(routine.getCreatedAt().toString())
                .active(routine.isActive())
                .trainerCuit(routine.getTrainer().getCuit())
                .clientCuit(routine.getClient().getCuit())
                .exerciseSets(routine.getExerciseSets()
                        .stream()
                        .map(ExerciseSetMapper::toDto)
                        .toList()
                )
                .trainingDiaries(routine.getTrainingDiaries()
                        .stream()
                        .map(TrainingDiaryMapper::toDto)
                        .toList()
                )
                .build();
    }

    public Routine toEntity(RoutineRequestDto dto) {
        return Routine.builder()
                .goals(dto.getGoals())
                .build();
    }
}
