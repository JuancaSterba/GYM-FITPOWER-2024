package com.gym.fit_power.repository;

import com.gym.fit_power.model.TrainingDiary;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface TrainingDiaryRepository extends JpaRepository<TrainingDiary, Long> {
    List<TrainingDiary> findByRoutineId(Long routineId);
}