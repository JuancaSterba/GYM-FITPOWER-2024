package com.gym.fit_power.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ExerciseResponseDto {

    private Long id;
    private String name;
    private String description;
    private String muscleGroup;
    private boolean enabled;

}
