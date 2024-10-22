package com.gym.fit_power.repository;

import com.gym.fit_power.model.Trainer;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface TrainerRepository extends JpaRepository<Trainer, Long> {
    Optional<Trainer> findByDni(String dni);
    boolean existsByDni(String dni);
    Optional<Trainer> findByEmail(String email);
    boolean existsByEmail(String email);
}
