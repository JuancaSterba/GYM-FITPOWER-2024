package com.gym.fit_power.dto;

import com.gym.fit_power.model.Client;
import lombok.Data;
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
    private String birthDate;

    public ClientDTO toDto(Client entity) {
        ClientDTO dto = new ClientDTO();
        dto.setCuit(entity.getCuit());
        dto.setAssignedGym(entity.getAssignedGym().getAddress());
        dto.setName(entity.getName());
        dto.setLastname(entity.getLastname());
        dto.setEmail(entity.getEmail());
        dto.setPhone(entity.getPhone());
        dto.setBirthDate(entity.getBirthDate().toString());
        return dto;
    }

}
