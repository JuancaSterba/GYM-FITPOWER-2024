package com.gym.fit_power.service;

import com.gym.fit_power.dto.NutriPlanDTO;


public interface NutriPlanService {

    NutriPlanDTO create(NutriPlanDTO request);

    NutriPlanDTO readOne(Long id);

    NutriPlanDTO disable(Long id);

    NutriPlanDTO enable(Long id);


}
