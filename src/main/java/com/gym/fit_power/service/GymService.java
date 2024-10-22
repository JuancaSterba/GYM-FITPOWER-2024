package com.gym.fit_power.service;

import com.gym.fit_power.model.Gym;
import com.gym.fit_power.dto.GymDTO;

public interface GymService extends ServiceCRUD<GymDTO, GymDTO>{
    Gym toEntity(GymDTO dto);
    GymDTO toDTO(Gym gym);
}
