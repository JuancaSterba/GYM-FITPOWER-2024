package com.gym.fit_power.repository;

import com.gym.fit_power.model.NutritionDiary;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface NutritioDiaryRepository extends JpaRepository<NutritionDiary, Long> {

}
