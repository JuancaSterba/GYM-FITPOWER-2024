package com.gym.fit_power.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.GregorianCalendar;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor

public class ResponseNutri {
    @JsonIgnore
    private Long id;
    private String name;
    private String lastname;
    private String cuit;
    private String email;
    private String phone;
    private String createdAt;

}
