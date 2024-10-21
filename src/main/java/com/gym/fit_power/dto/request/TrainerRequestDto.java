package com.gym.fit_power.dto.request;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class TrainerRequestDto {

    @NotBlank(message = "{dni.notBlank}")
    @Pattern(regexp = "^\\d{8}$", message = "{dni.invalid}")
    private String dni;

    @NotBlank(message = "{name.notBlank}")
    @Size(min = 2, max = 50, message = "{name.invalid}")
    private String name;

    @NotBlank(message = "{lastname.notBlank}")
    @Size(min = 2, max = 50, message = "{lastname.invalid}")
    private String lastname;

    @NotBlank(message = "{email.notBlank}")
    @Email(message = "{email.invalid}")
    @Size(min = 5, max = 100, message = "{email.size.invalid}")
    private String email;

    @NotBlank(message = "{phoneNumber.notBlank}")
    @Pattern(regexp = "^\\d{10}$", message = "{phoneNumber.invalid}")
    private String phoneNumber;

    @NotBlank(message = "{speciality.notBlank}")
    @Size(min = 2, max = 50, message = "{speciality.invalid}")
    private String speciality;

    @NotBlank(message = "{birthDate.notBlank}")
    @Pattern(regexp = "^\\d{1,2}/\\d{1,2}/\\d{4}$", message = "{birthDate.invalid}")
    private String birthDate;

}
