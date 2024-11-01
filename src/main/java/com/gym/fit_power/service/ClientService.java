package com.gym.fit_power.service;

import java.util.List;
import com.gym.fit_power.model.Client;
import com.gym.fit_power.dto.ClientDTO;


public interface ClientService extends ServiceCRUD<ClientDTO, ClientDTO> {

    ClientDTO readByCuit(String cuit);

    List<ClientDTO> readByGym(String gymCode);

    ClientDTO changeGym(String clientCuit, String gymCode);

    Client toEntity(ClientDTO dto);

    ClientDTO toDTO(Client entity);
}
