package com.gym.fit_power.repository;

import com.gym.fit_power.model.Client;
import org.springframework.stereotype.Repository;
import org.springframework.data.jpa.repository.JpaRepository;


@Repository
public interface ClientRepository extends JpaRepository<Client, Long>{

}
