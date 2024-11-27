package com.gym.fit_power.dto.request;

import jakarta.persistence.Column;
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

    @NotBlank(message = "{cuit.notBlank}")
    @Pattern(regexp = "^\\d{8}$", message = "{cuit.invalid}")
    private String cuit;

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
  
}
