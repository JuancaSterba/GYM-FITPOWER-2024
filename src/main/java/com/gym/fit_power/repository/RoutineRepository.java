package com.gym.fit_power.repository;

import com.gym.fit_power.model.Routine;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface RoutineRepository extends JpaRepository<Routine, Long> {
}
