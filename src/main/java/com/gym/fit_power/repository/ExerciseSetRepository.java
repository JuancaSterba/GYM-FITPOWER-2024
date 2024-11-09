package com.gym.fit_power.repository;

import com.gym.fit_power.model.ExerciseSet;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface ExerciseSetRepository extends JpaRepository<ExerciseSet, Long> {
    List<ExerciseSet> findAllByRoutineId(Long routineId);
    Optional<ExerciseSet> findByRoutineIdAndExerciseId(Long routineId, Long exerciseId);
}