package com.gym.fit_power.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ExerciseSetResponseDto {

    private Long id;
    private int reps;
    private int sets;
    private int restInMinutes;
    private Long routineId;
    private Long exerciseId;

}
