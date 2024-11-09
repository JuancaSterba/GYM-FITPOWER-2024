package com.gym.fit_power.service.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import lombok.extern.slf4j.Slf4j;
import com.gym.fit_power.model.Gym;
import com.gym.fit_power.model.Client;
import com.gym.fit_power.dto.ClientDTO;
import org.springframework.stereotype.Service;
import com.gym.fit_power.service.ClientService;
import com.gym.fit_power.repository.GymRepository;
import org.springframework.dao.DataAccessException;
import com.gym.fit_power.repository.ClientRepository;
import com.gym.fit_power.util.MyGregorianCalendarConverter;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import static com.gym.fit_power.constant.ClientConstants.*;

@Slf4j
@Service
public class ClientServiceImpl implements ClientService {

    GymRepository gymRepository;
    ClientRepository clientRepository;
    MyGregorianCalendarConverter converter = new MyGregorianCalendarConverter();
    protected static final Logger logger = LoggerFactory.getLogger(ClientServiceImpl.class);

    public ClientServiceImpl(ClientRepository clientRepository, GymRepository gymRepository) {
        this.gymRepository = gymRepository;
        this.clientRepository = clientRepository;
    }

    @Override
    public ClientDTO create(ClientDTO clientDTO) throws DataAccessException {
        try {
            Client newClient = toEntity(clientDTO);
            newInfoLog("Save the new client: " + newClient.getCuit());
            newClient.setEnabled(true);
            return toDTO(clientRepository.save(newClient));
        } catch (Exception e) {
            clientCouldNotBE(clientDTO.getCuit(), "saved", e);
        }
        return null;
    }

    @Override
    public ClientDTO readOne(Long id) throws DataAccessException {
        try {
            newInfoLog("Searching the client with id: " + id);
            return toDTO(clientRepository.findById(id).orElseThrow());
        } catch (Exception e) {
            errorSearch(e);
        }
        return null;
    }

    @Override
    public ClientDTO readByCuit(String cuit) throws DataAccessException {
        try {
            newInfoLog("Searching the client with cuit: " + cuit);
            return toDTO(clientRepository.findAll().stream()
                    .filter(client -> client.getCuit().equals(cuit)).findFirst().orElseThrow());
        } catch (Exception e) {
            errorSearch(e);

        }
        return null;
    }

    @Override
    public List<ClientDTO> readAll() throws DataAccessException {
        try {
            List<ClientDTO> response = new ArrayList<>();
            newInfoLog("Searching all clients on the database");
            for (Client client : clientRepository.findAll()) {
                response.add(toDTO(client));
            }
            return response;
        } catch (Exception e) {
            errorSearch(e);
        }
        return new ArrayList<>();
    }

    @Override
    public ClientDTO update(Long id, ClientDTO clientDTO) throws DataAccessException {
        try {
            newInfoLog("Updating client with cuit: " + clientDTO.getCuit());
            Client oldClient = toEntity(readOne(id));
            Client newClient = toEntity(clientDTO);
            oldClient.setName(newClient.getName());
            oldClient.setLastname(newClient.getLastname());
            oldClient.setEmail(newClient.getEmail());
            oldClient.setPhone(newClient.getPhone());
            oldClient.setBirthDate(newClient.getBirthDate());
            return toDTO(clientRepository.save(oldClient));
        } catch (Exception e) {
            clientCouldNotBE(clientDTO.getCuit(), "updated", e);
        }
        return null;
    }

    @Override
    public ClientDTO changeGym(String clientCuit, String gymAddress) {
        try {
            newInfoLog("Change the gym of client: " + clientCuit);
            Client client = toEntity(readByCuit(clientCuit));
            if (client.getAssignedGym().getAddress().equals(gymAddress)) {
                throw new Exception(); //TODO manejo de excepciones
            } else {
                client.setAssignedGym(verifyGym(gymAddress));
                clientRepository.save(client);
            }
        } catch (Exception e) {
            newErrorLog("Error: ", e);
        }
        return null;
    }

    @Override
    public ClientDTO disable(Long id) throws DataAccessException {
        try {
            Client entity = toEntity(readOne(id));
            newInfoLog("Disabling the client with cuit: " + entity.getCuit());
            entity.setEnabled(false);
            return toDTO(clientRepository.save(entity));
        } catch (Exception e) {
            clientCouldNotBE(id.toString(), "disabled", e);
        }
        return null;
    }

    @Override
    public ClientDTO enable(Long id) throws DataAccessException {
        try {
            newInfoLog("Enabling the client with id " + id);
            Client entity = toEntity(readOne(id));
            entity.setEnabled(true);
            return toDTO(clientRepository.save(entity));
        } catch (Exception e) {
            clientCouldNotBE(id.toString(), "enabled", e);
        }
        return null;
    }

    @Override
    public Client toEntity(ClientDTO dto) {
        Client entity = new Client();
        entity.setCuit(dto.getCuit());
        entity.setName(dto.getName());
        entity.setLastname(dto.getLastname());
        entity.setEmail(dto.getEmail());
        entity.setPhone(dto.getPhone());
        entity.setBirthDate(LocalDate.now());//TODO se requiere conversion
        return entity;
    }

    @Override
    public ClientDTO toDTO(Client entity) {
        ClientDTO dto = new ClientDTO();
        dto.setId(entity.getId());
        dto.setCuit(entity.getCuit());
        dto.setAssignedGym(entity.getAssignedGym());
        dto.setName(entity.getName());
        dto.setLastname(entity.getLastname());
        dto.setEmail(entity.getEmail());
        dto.setPhone(entity.getPhone());
        dto.setBirthDate(entity.getBirthDate().toString());
        dto.setEnabled(entity.getEnabled());
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

    private void newInfoLog(String description) {
        logger.info(SERVICE, description);
    }

    private void newErrorLog(String description, Exception e) {
        logger.error(SERVICE + " {}", description, e.getMessage());
    }

    private void errorSearch(Exception e) {
        newErrorLog(SEARCH_ERROR, e);
    }

    private void clientCouldNotBE(String cuit, String task, Exception e) {
        logger.error(CLIENT_COULD_NOT_BE, cuit, task, e);
    }

}
