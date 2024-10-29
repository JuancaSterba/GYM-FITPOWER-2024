package com.gym.fit_power.dto;

import lombok.Data;
import lombok.NoArgsConstructor;
import com.fasterxml.jackson.annotation.JsonIgnore;

@Data
@NoArgsConstructor
public class GymDTO {

    @JsonIgnore
    private Long id;

    private String code;
    private String domain;
    private String address;
    private String mail;
    private String phone;
    private String createdAt;
    private String updatedAt;
    private Boolean enabled;

}
