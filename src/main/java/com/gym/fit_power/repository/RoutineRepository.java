package com.gym.fit_power.repository;

import com.gym.fit_power.model.Client;
import com.gym.fit_power.model.Routine;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface RoutineRepository extends JpaRepository<Routine, Long> {
    Optional<Routine> findByClientAndActiveTrue(Client client);
}
