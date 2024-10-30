package com.gym.fit_power.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.GregorianCalendar;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class RequestNutri {
    @JsonIgnore
    private Long id;
    private String name;
    private String lastname;
    private String cuit;
    private String email;
    private String phone;
    private String speciality;
    private String birthdate;

}
