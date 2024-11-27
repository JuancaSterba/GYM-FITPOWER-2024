package com.gym.fit_power.service;

import com.gym.fit_power.dto.NutritionDiaryDTO;

import java.util.List;

public interface NutritionDiaryService {
    
    NutritionDiaryDTO update (String cuit,NutritionDiaryDTO request);
    List<NutritionDiaryDTO> readByClientActivePlan(String clientCuit);
    public List<NutritionDiaryDTO> readByNutritionPlan(String cuit, Long id);
}
