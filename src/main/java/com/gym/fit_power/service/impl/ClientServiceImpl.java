package com.gym.fit_power.service.impl;

import com.gym.fit_power.notification.PublishClientEvents;
import lombok.extern.slf4j.Slf4j;
import com.gym.fit_power.model.Gym;
import com.gym.fit_power.model.Client;
import com.gym.fit_power.dto.ClientDTO;
import org.springframework.stereotype.Service;
import com.gym.fit_power.service.ClientService;
import com.gym.fit_power.repository.GymRepository;
import org.springframework.dao.DataAccessException;
import com.gym.fit_power.repository.ClientRepository;

import java.util.List;
import java.util.ArrayList;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

@Slf4j
@Service
public class ClientServiceImpl implements ClientService {

    GymRepository gymRepository;
    ClientRepository clientRepository;
    PublishClientEvents clientEventPublisher;

    public ClientServiceImpl(ClientRepository clientRepository, GymRepository gymRepository,
                             PublishClientEvents clientEventPublisher) {
        this.gymRepository = gymRepository;
        this.clientRepository = clientRepository;
        this.clientEventPublisher = clientEventPublisher;
    }

    @Override
    public ClientDTO create(ClientDTO clientDTO) throws DataAccessException {
        try {
            log.info("Save the new client: {}", clientDTO.getCuit());
            if (verifyGym(clientDTO.getAssignedGym()) == null) {
                throw new Exception("no existe el gimnasio asignado.");//TODO manejo de excepciones
            }
            Client newClient = toEntity(clientDTO);
            newClient.setEnabled(true);
            Client created = clientRepository.save(newClient);
            clientEventPublisher.publishCreate(created);
            return toDTO(created);
        } catch (Exception e) {
            log.error("The client {} could not be saved. Error: ", clientDTO.getCuit(), e);
        }
        return null;
    }

    private Client readOne(String cuit) throws DataAccessException {
        Client result = new Client();
        try {
            log.info("Searching the client {} in the database", cuit);
            result = clientRepository.findByCuit(cuit);
            if (result == null) {
                throw new Exception("no existe el cliente solicitado.");//TODO manejo de excepciones
            }
        } catch (Exception e) {
            log.error("An error has occurred while searching the client. Error: ",e);
        }
        return result;
    }

    @Override
    public ClientDTO readByCuit(String cuit) throws DataAccessException {
        try {
            log.info("Searching the client with cuit: {}", cuit);
            return toDTO(clientRepository.findByCuit(cuit));
        } catch (Exception e) {
            log.error("An error has occurred while searching the client. Error: ",e);
        }
        return null;
    }

    @Override
    public List<ClientDTO> readAll() throws DataAccessException {
        try {
            List<ClientDTO> response = new ArrayList<>();
            log.info("Searching all clients on the database");
            for (Client client : clientRepository.findAll()) {
                response.add(toDTO(client));
            }
            return response;
        } catch (Exception e) {
            log.error("There are no clients in the database. Error: ",e);
        }
        return new ArrayList<>();
    }

    @Override
    public ClientDTO update(String cuit, ClientDTO clientDTO) throws DataAccessException {
        try {
            log.info("Updating client with cuit: {}", cuit);
            Client oldClient = readOne(cuit);
            Client newClient = toEntity(clientDTO);
            oldClient.setName(newClient.getName());
            oldClient.setLastname(newClient.getLastname());
            oldClient.setEmail(newClient.getEmail());
            oldClient.setPhone(newClient.getPhone());
            oldClient.setBirthDate(newClient.getBirthDate());
            return toDTO(clientRepository.save(oldClient));
        } catch (Exception e) {
            log.error("The client {} could not be updated. Error: ", clientDTO.getCuit(), e);
        }
        return null;
    }

    @Override
    public ClientDTO changeGym(String clientCuit, String gymAddress) {
        try {
            log.info("Change the gym of the client: {}", clientCuit);
            Client client = toEntity(readByCuit(clientCuit));
            if (client.getAssignedGym().getAddress().equals(gymAddress)) {
                throw new Exception("no puede cambiarse al mismo gimnasio"); //TODO manejo de excepciones
            } else if (verifyGym(client.getAssignedGym().getAddress()) == null) {
                throw new Exception("no existe el gimnasio solicitado.");//TODO manejo de excepciones
            } else {
                client.setAssignedGym(verifyGym(gymAddress));
                clientRepository.save(client);
            }
        } catch (Exception e) {
            log.error("Error: ", e);
        }
        return null;
    }

    @Override
    public ClientDTO disable(String cuit) throws DataAccessException {
        try {
            Client entity = readOne(cuit);
            log.info("Disabling the client with cuit: {}", entity.getCuit());
            entity.setEnabled(false);
            return toDTO(clientRepository.save(entity));
        } catch (Exception e) {
            log.error("The client {} could not be disabled. Error: ", cuit, e);
        }
        return null;
    }

    @Override
    public ClientDTO enable(String cuit) throws DataAccessException {
        try {
            log.info("Enabling the client with cuit {}", cuit);
            Client entity = readOne(cuit);
            entity.setEnabled(true);
            return toDTO(clientRepository.save(entity));
        } catch (Exception e) {
            log.error("The client {} could not be enabled. Error: ", cuit, e);
        }
        return null;
    }

    @Override
    public Client toEntity(ClientDTO dto) {
        Client entity = new Client();
        entity.setCuit(dto.getCuit());
        entity.setAssignedGym(verifyGym(dto.getAssignedGym()));
        entity.setName(dto.getName());
        entity.setLastname(dto.getLastname());
        entity.setEmail(dto.getEmail());
        entity.setPhone(dto.getPhone());
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        entity.setBirthDate(LocalDate.parse(dto.getBirthDate(), formatter));
        return entity;
    }

    @Override
    public ClientDTO toDTO(Client entity) {
        ClientDTO dto = new ClientDTO();
        dto.setCuit(entity.getCuit());
        dto.setAssignedGym(entity.getAssignedGym().getAddress());
        dto.setName(entity.getName());
        dto.setLastname(entity.getLastname());
        dto.setEmail(entity.getEmail());
        dto.setPhone(entity.getPhone());
        dto.setBirthDate(entity.getBirthDate().toString());
        return dto;
    }

    private Gym verifyGym(String address) {
        for (Gym gym : gymRepository.findAll()) {
            if (gym.getAddress().equals(address)) {
                return gym;
            }
        }
        return null;
    }

}
