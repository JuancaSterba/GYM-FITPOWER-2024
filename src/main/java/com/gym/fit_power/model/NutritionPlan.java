package com.gym.fit_power.model;

import lombok.Data;

import java.time.LocalDate;
import java.util.List;
import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "nutrition_plan")
public class NutritionPlan {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private LocalDate creatAt;
    private Float dailyCalories;
    private Float dailyCarbohydrates;
    private Float dailyProteins;
    private Float dailyFats;
    private Float desiredWeight;
    private Boolean enabled;
    private Nutritionist nutritionist;

    @ManyToOne
    @JoinColumn(name = "client_id")
    private Client client;
    @OneToMany(mappedBy = "nutrition_plan")
    private List<NutritionDiary> logNutri;


}
