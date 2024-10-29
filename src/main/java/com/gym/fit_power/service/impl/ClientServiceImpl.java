package com.gym.fit_power.service.impl;

import com.gym.fit_power.model.Gym;
import com.gym.fit_power.model.Client;
import com.gym.fit_power.dto.ClientDTO;
import com.gym.fit_power.repository.ClientRepository;
import com.gym.fit_power.repository.GymRepository;
import com.gym.fit_power.service.ClientService;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.List;

import static com.gym.fit_power.constant.ClientConstants.*;

@Slf4j
@Service
public class ClientServiceImpl implements ClientService {

    GymRepository gymRepository;
    ClientRepository clientRepository;
    protected static final Logger logger = LoggerFactory.getLogger(ClientServiceImpl.class);

    public ClientServiceImpl(ClientRepository clientRepository, GymRepository gymRepository) {
        this.clientRepository = clientRepository;
        this.gymRepository = gymRepository;
    }

    @Override
    public ClientDTO create(ClientDTO clientDTO) throws DataAccessException {
        try {
            Client newClient = toEntity(clientDTO);
            newInfoLog("Save the new client: " + newClient.getCuit());
            newClient.setEnabled(true);
            newClient.setCreatedAt(new GregorianCalendar());
            newClient.setUpdatedAt(new GregorianCalendar());
            return toDTO(clientRepository.save(newClient));
        } catch (Exception e) {
            newErrorLog(clientCouldNotBE(clientDTO.getCuit()) + "saved. Error:", e);
        }
        return null;
    }

    @Override
    public ClientDTO readOne(Long id) throws DataAccessException {
        try {
            newInfoLog("Searching the client with id: " + id);
            return toDTO(clientRepository.findById(id).orElseThrow());
        } catch (Exception e) {
            newErrorLog(SEARCH_ERROR, e);
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
            newErrorLog(SEARCH_ERROR, e);

        }
        return null;
    }

    @Override
    public List<ClientDTO> readByGym(String gymCode) {
        try {
            List<ClientDTO> response = new ArrayList<>();
            newInfoLog("Searching all clients on the gym: " + gymCode);
            for (Client client : clientRepository.findAll()) {
                if (client.getAssignedGym().getCode().equals(gymCode)) {
                    response.add(toDTO(client));
                }
            }
            return response;
        } catch (Exception e) {
            newErrorLog(SEARCH_ERROR, e);
        }
        return new ArrayList<>();
    }

    @Override
    public List<ClientDTO> readByTrainer(String trainerCuit) {
        //TODO esperando la asignación con entrenador
        return new ArrayList<>();
    }

    @Override
    public List<ClientDTO> readByNutritionist(String nutritionistCuit) {
        //TODO esperando la asignación con nutricionista
        return new ArrayList<>();
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
            newErrorLog(SEARCH_ERROR, e);
        }
        return new ArrayList<>();
    }

    @Override
    public ClientDTO update(ClientDTO clientDTO) throws DataAccessException {
        try {
            newInfoLog("Updating personal data of client with cuit: " + clientDTO.getCuit());
            Client oldClient = toEntity(readByCuit(clientDTO.getCuit()));
            Client newClient = toEntity(clientDTO);
            oldClient.setName(newClient.getName());
            oldClient.setLastname(newClient.getLastname());
            oldClient.setEmail(newClient.getEmail());
            oldClient.setPhone(newClient.getPhone());
            oldClient.setGenre(newClient.getGenre());
            oldClient.setBirthDate(newClient.getBirthDate());
            oldClient.setUpdatedAt(new GregorianCalendar());
            return toDTO(clientRepository.save(oldClient));
        } catch (Exception e) {
            newErrorLog(clientCouldNotBE(clientDTO.getCuit()) + "updated. Error:", e);
        }
        return null;
    }

    @Override
    public ClientDTO updatePhysics(ClientDTO clientDTO) {
        try {
            newInfoLog("Updating physic data of client with cuit: " + clientDTO.getCuit());
            Client oldClient = toEntity(readByCuit(clientDTO.getCuit()));
            Client newClient = toEntity(clientDTO);
            oldClient.setPhysicalComposition(newClient.getPhysicalComposition());
            oldClient.setWeight(newClient.getWeight());
            oldClient.setHeight(newClient.getHeight());
            oldClient.setCorpulence(newClient.getCorpulence());
            oldClient.setUpdatedAt(new GregorianCalendar());
            return toDTO(clientRepository.save(oldClient));
        } catch (Exception e) {
            newErrorLog(clientCouldNotBE(clientDTO.getCuit()) + "updated. Error:", e);
        }
        return null;
    }

    @Override
    public ClientDTO updateAllergies(ClientDTO clientDTO, List<String> allergies) {
        try {
            newInfoLog("Updating allergies list of client with cuit: " + clientDTO.getCuit());
            Client oldClient = toEntity(readByCuit(clientDTO.getCuit()));
            oldClient.setAllergies(allergies);
            return toDTO(clientRepository.save(oldClient));
        } catch (Exception e) {
            newErrorLog(clientCouldNotBE(clientDTO.getCuit()) + "updated. Error:", e);
        }
        return null;
    }

    @Override
    public ClientDTO changeGym(ClientDTO clientDTO, String gymCode) {
        try {
            newInfoLog("Change the gym of client: " + clientDTO.getCuit());
            Client client = toEntity(clientDTO);
            if (client.getAssignedGym().getCode().equals(gymCode)) {
                throw new Exception(); //TODO manejo de excepciones
            } else {
                client.setAssignedGym(verifyGym(gymCode));
                clientRepository.save(client);
            }
        } catch (Exception e) {
            newErrorLog("Error: ", e);
        }
        return null;
    }

    @Override
    public ClientDTO assignTrainer(ClientDTO clientDTO, String trainerCuit) {
        try {
            newInfoLog("Assign a trainer to client: " + clientDTO.getCuit());
            Client client = toEntity(clientDTO);
            return toDTO(client); //TODO se requiere repositorio de entrenadores
        } catch (Exception e) {
            newErrorLog("Error: ", e);
        }
        return null;
    }

    @Override
    public ClientDTO changeTrainer(ClientDTO clientDTO, String trainerCuit) {
        try {
            newInfoLog("Change the trainer of client: " + clientDTO.getCuit());
            Client client = toEntity(clientDTO);
            return toDTO(client); //TODO se requiere repositorio de entrenadores
        } catch (Exception e) {
            newErrorLog("Error: ", e);
        }
        return null;
    }

    @Override
    public ClientDTO resignTrainer(ClientDTO clientDTO) {
        try {
            newInfoLog("Unassign the trainer of client: " + clientDTO.getCuit());
            Client client = toEntity(clientDTO);
            return toDTO(client); //TODO se requiere repositorio de entrenadores
        } catch (Exception e) {
            newErrorLog("Error: ", e);
        }
        return null;
    }

    @Override
    public ClientDTO assignNutritionist(ClientDTO clientDTO, String nutritionistCuit) {
        try {
            newInfoLog("Assign a nutritionist to client: " + clientDTO.getCuit());
            Client client = toEntity(clientDTO);
            return toDTO(client); //TODO se requiere repositorio de entrenadores
        } catch (Exception e) {
            newErrorLog("Error: ", e);
        }
        return null;
    }

    @Override
    public ClientDTO changeNutritionist(ClientDTO clientDTO, String nutritionistCuit) {
        try {
            newInfoLog("Change the nutritionist of client: " + clientDTO.getCuit());
            Client client = toEntity(clientDTO);
            return toDTO(client); //TODO se requiere repositorio de entrenadores
        } catch (Exception e) {
            newErrorLog("Error: ", e);
        }
        return null;
    }

    @Override
    public ClientDTO resignNutritionist(ClientDTO clientDTO) {
        try {
            newInfoLog("Unassign the nutritionist of client: " + clientDTO.getCuit());
            Client client = toEntity(clientDTO);
            return toDTO(client); //TODO se requiere repositorio de entrenadores
        } catch (Exception e) {
            newErrorLog("Error: ", e);
        }
        return null;
    }

    @Override
    public ClientDTO disable(ClientDTO clientDTO) throws DataAccessException {
        try {
            newInfoLog("Disabling the client with cuit: " + clientDTO.getCuit());
            Client entity = toEntity(clientDTO);
            entity.setEnabled(false);
            return toDTO(clientRepository.save(entity));
        } catch (Exception e) {
            newErrorLog(clientCouldNotBE(clientDTO.getCuit()) + "disabled. Error:", e);
        }
        return null;
    }

    @Override
    public ClientDTO enable(ClientDTO clientDTO) throws DataAccessException {
        try {
            newInfoLog("Enabling the client with cuit " + clientDTO.getCuit());
            Client entity = toEntity(clientDTO);
            entity.setEnabled(true);
            return toDTO(clientRepository.save(entity));
        } catch (Exception e) {
            newErrorLog(clientCouldNotBE(clientDTO.getCuit()) + "enabled. Error:", e);
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
        entity.setGenre(dto.getGenre());
        entity.setPhysicalComposition(dto.getPhysicalComposition());
        entity.setWeight(dto.getWeight());
        entity.setHeight(dto.getHeight());
        entity.setCorpulence(dto.getCorpulence());
        entity.setAllergies(dto.getAllergies());
        entity.setBirthDate(new GregorianCalendar()); //TODO se requiere conversion
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
        dto.setGenre(entity.getGenre());
        dto.setPhysicalComposition(entity.getPhysicalComposition());
        dto.setWeight(entity.getWeight());
        dto.setHeight(entity.getHeight());
        dto.setCorpulence(entity.getCorpulence());
        dto.setAllergies(entity.getAllergies());
        dto.setBirthDate(entity.getBirthDate().getTime().toString());
        dto.setCreatedAt(entity.getCreatedAt().getTime().toString());
        dto.setUpdatedAt(entity.getUpdatedAt().getTime().toString());
        dto.setEnabled(entity.getEnabled());
        return dto;
    }

    private Gym verifyGym(String gymCode) {
        for (Gym gym : gymRepository.findAll()) {
            if (gym.getCode().equals(gymCode)) {
                return gym;
            }
        }
        return null;
    }

    private String clientCouldNotBE(String cuit) {
        return "The client " + cuit + " could not be";
    }

    private void newInfoLog(String description) {
        logger.info(SERVICE, description);
    }

    private void newErrorLog(String description, Exception e) {
        logger.error(SERVICE + " {}", description, e.getMessage());
    }

}
