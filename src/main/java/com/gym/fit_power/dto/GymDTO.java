package com.gym.fit_power.dto;

import lombok.Data;
import lombok.NoArgsConstructor;
import com.fasterxml.jackson.annotation.JsonIgnore;


@Data
@NoArgsConstructor
public class GymDTO {

    @JsonIgnore
    Long id;

    String code;
    String domain;
    String address;
    String mail;
    String phone;
    String createdAt;
    String updatedAt;
    Boolean active;

}
