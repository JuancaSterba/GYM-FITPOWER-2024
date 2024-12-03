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

    @NotBlank(message = "{commentary.notBlank}")
    @Size(min = 3, max = 500, message = "{commentary.size.invalid}")
    private String commentary;

}
