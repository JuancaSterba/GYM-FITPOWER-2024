package com.gym.fit_power.service;

import com.gym.fit_power.dto.NutritionDiaryDTO;

public interface NutritionDiaryService {

    NutritionDiaryDTO create (NutritionDiaryDTO request);
    NutritionDiaryDTO update (Long id,NutritionDiaryDTO request);

}
