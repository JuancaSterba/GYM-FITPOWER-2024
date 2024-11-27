package com.gym.fit_power.dto.request;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ExerciseSetRequestDto {

    @NotNull(message = "{reps.notNull}")
    @Min(value = 1, message = "{reps.min.invalid}")
    @Pattern(regexp = "^\\d+$", message = "{reps.invalid}")
    private int reps;

    @NotNull(message = "{sets.notNull}")
    @Min(value = 1, message = "{sets.min.invalid}")
    @Pattern(regexp = "^\\d+$", message = "{sets.invalid}")
    private int sets;

    @NotNull(message = "{restInMinutes.notNull}")
    @Min(value = 1, message = "{restInMinutes.min.invalid}")
    @Pattern(regexp = "^\\d+$", message = "{restInMinutes.invalid}")
    private int restInMinutes;

    @NotNull(message = "{routineId.notNull}")
    @Pattern(regexp = "^\\d+$", message = "{routineId.invalid}")
    private Long routineId;

    @NotNull(message = "{exerciseId.notNull}")
    @Pattern(regexp = "^\\d+$", message = "{exerciseId.invalid}")
    private Long exerciseId;

}
