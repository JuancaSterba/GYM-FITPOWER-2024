package com.gym.fit_power.repository;

import com.gym.fit_power.model.Nutritionist;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface NutriRepository extends JpaRepository<Nutritionist,Long> {
}
