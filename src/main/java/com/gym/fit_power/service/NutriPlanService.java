package com.gym.fit_power.service;

import com.gym.fit_power.dto.NutriPlanDTO;

import java.util.List;


public interface NutriPlanService {

    NutriPlanDTO create(NutriPlanDTO request);

    NutriPlanDTO readOne(Long id);

    List<NutriPlanDTO> readByClient(String clientCuit);

    NutriPlanDTO readPlanActiveByClient(String clientCuit);

    NutriPlanDTO readPlanByClient(String clientCuit, Long id);


}
