package com.gym.fit_power.repository;

import com.gym.fit_power.model.TrainingDiary;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TrainingDiaryRepository extends JpaRepository<TrainingDiary, Long> {
}