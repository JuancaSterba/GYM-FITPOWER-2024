package com.gym.fit_power.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@AllArgsConstructor
@NoArgsConstructor
public class NutritionDiaryDTO {
    @JsonIgnore
    private Long id;
    @JsonIgnore
    private Long idNutritionPlan;
    private String updateAt;
    private String breakfast;
    private String lunch;
    private String snacks;
    private String dinner;
    private Float actualWeight;
    private String comentary;




}
