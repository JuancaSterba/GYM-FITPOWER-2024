package com.gym.fit_power.dto.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class TrainerResponseDto {
    private Long id;
    private String dni;
    private String name;
    private String lastname;
    private String email;
    private String phoneNumber;
    private String speciality;
    private String birthDate;
    private String createdAt;
    private boolean enabled;
}
