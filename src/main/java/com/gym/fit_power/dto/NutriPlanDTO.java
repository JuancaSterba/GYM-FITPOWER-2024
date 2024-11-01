package com.gym.fit_power.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.gym.fit_power.model.NutritionDiary;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class NutriPlanDTO {
    @JsonIgnore
    private Long id;
    private Float dailyCalories;
    private Float dailyCarbohydrates;
    private Float dailyProteins;
    private Float dailyFats;
    private Float desiredWeight;
    private String nutritionistCuit;
    private String ClientCuit;


}
