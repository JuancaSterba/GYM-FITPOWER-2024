package com.gym.fit_power.dto.request;

import jakarta.validation.constraints.NotBlank;
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

    @NotNull
    private Long trainerId;

    @NotNull
    private Long customerId;

    private List<ExerciseSetRequestDto> exerciseSets;

    private List<TrainingDiaryRequestDto> trainingDiaries;


}
