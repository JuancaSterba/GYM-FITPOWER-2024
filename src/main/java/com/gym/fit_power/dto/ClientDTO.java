package com.gym.fit_power.dto;

import lombok.Data;
import lombok.NoArgsConstructor;
import com.gym.fit_power.model.Gym;
import com.fasterxml.jackson.annotation.JsonIgnore;

@Data
@NoArgsConstructor
public class ClientDTO {

    @JsonIgnore
    private Long id;

    private String cuit;
    private Gym assignedGym;
    private String name;
    private String lastname;
    private String email;
    private String phone;
    private String birthDate;
    private Boolean enabled;

}
