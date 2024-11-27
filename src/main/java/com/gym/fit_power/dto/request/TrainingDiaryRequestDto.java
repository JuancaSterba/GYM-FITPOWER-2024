package com.gym.fit_power.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class TrainingDiaryRequestDto {

    @NotBlank(message = "{comentary.notBlank}")
    @Size(min = 3, max = 500, message = "{comentary.size.invalid}")
    private String comentary;

    @NotBlank(message = "{routineId.notBlank}")
    private Long routineId;

}
