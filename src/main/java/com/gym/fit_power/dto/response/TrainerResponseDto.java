package com.gym.fit_power.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TrainerResponseDto {
    private Long id;
    private String cuit;
    private String name;
    private String lastname;
    private String email;
    private String phoneNumber;
    private String createdAt;
    private boolean enabled;
}
