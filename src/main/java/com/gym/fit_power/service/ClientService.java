package com.gym.fit_power.service;

import java.util.List;
import com.gym.fit_power.model.Client;
import com.gym.fit_power.dto.ClientDTO;

public interface ClientService extends ServiceCRUD<ClientDTO, ClientDTO> {

    ClientDTO readByCuit(String cuit);

    List<ClientDTO> readByGym(String gymCode);

    List<ClientDTO> readByTrainer(String trainerCuit);

    List<ClientDTO> readByNutritionist(String nutritionistCuit);

    ClientDTO updatePhysics(ClientDTO clientDTO);

    ClientDTO updateAllergies(ClientDTO clientDTO, List<String> allergies);

    ClientDTO changeGym(ClientDTO clientDTO, String gymCode);

    ClientDTO assignTrainer(ClientDTO clientDTO, String trainerCuit);

    ClientDTO changeTrainer(ClientDTO clientDTO, String trainerCuit);

    ClientDTO resignTrainer(ClientDTO clientDTO);

    ClientDTO assignNutritionist(ClientDTO clientDTO, String nutritionistCuit);

    ClientDTO changeNutritionist(ClientDTO clientDTO, String nutritionistCuit);

    ClientDTO resignNutritionist(ClientDTO clientDTO);

    Client toEntity(ClientDTO dto);

    ClientDTO toDTO(Client client);
}
