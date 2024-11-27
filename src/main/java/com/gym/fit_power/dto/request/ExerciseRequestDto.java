package com.gym.fit_power.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ExerciseRequestDto {

    @NotBlank(message = "{name.notBlank}")
    @Size(min = 3, max = 50, message = "{name.invalid}")
    private String name;

    @NotBlank(message = "{description.notBlank}")
    @Size(min = 3, max = 500, message = "{description.invalid}")
    private String description;

    @NotBlank(message = "{muscleGroup.notBlank}")
    @Size(min = 3, max = 50, message = "{muscleGroup.invalid}")
    private String muscleGroup;
}
