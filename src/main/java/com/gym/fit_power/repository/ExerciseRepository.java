package com.gym.fit_power.repository;

import com.gym.fit_power.model.Exercise;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface ExerciseRepository extends JpaRepository<Exercise, Long> {
  Optional<Exercise> findByName(String name);
  boolean existsByName(String name);
  Optional<Exercise> findByMuscleGroup(String muscleGroup);
  boolean existsByMuscleGroup(String muscleGroup);
}