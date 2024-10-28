package com.gym.fit_power.dto;

import lombok.Data;
import java.util.List;
import lombok.NoArgsConstructor;
import com.fasterxml.jackson.annotation.JsonIgnore;

@Data
@NoArgsConstructor
public class ClientDTO {

    @JsonIgnore
    private Long id;

    private String cuit;
    private String assignedGym;
    private String name;
    private String lastname;
    private String email;
    private String phone;
    private String genre;
    private String physicalComposition;
    private Float weight;
    private Float height;
    private String corpulence;
    private List<String> allergies;
    private String birthDate;

}
