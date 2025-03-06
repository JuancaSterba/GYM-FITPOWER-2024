package com.gym.fit_power.repository;

import com.gym.fit_power.model.Gym;
import org.springframework.stereotype.Repository;
import org.springframework.data.jpa.repository.JpaRepository;

@Repository
public interface GymRepository extends JpaRepository<Gym, Long>{
    Gym findByAddress(String address);
}
