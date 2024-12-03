package com.gym.fit_power.dto.request;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class RoutineRequestDto {

    @NotBlank(message = "{goals.notBlank}")
    @Size(min = 10, max = 200, message = "{goals.size.invalid}")
    private String goals;

    @NotBlank(message = "{trainerCuit.notBlank}")
    @Size(min = 11, max = 11, message = "{trainerCuit.size.invalid}")
    private String trainerCuit;

    @NotEmpty(message = "{exerciseSets.notEmpty}")
    private List<ExerciseSetRequestDto> exerciseSets;
  
}
