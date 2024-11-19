package com.gym.fit_power.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class RoutineResponseDto {
    private Long id;
    private String goals;
    private String createdAt;
    private boolean active;
    private String trainerCuit;
    private String clientCuit;
    private List<ExerciseSetResponseDto> exerciseSets;
    private List<TrainingDiaryResponseDto> trainingDiaries;
}
