package com.gym.fit_power.service.impl;

import java.util.List;
import org.slf4j.Logger;

import java.util.ArrayList;

import org.slf4j.LoggerFactory;
import lombok.extern.slf4j.Slf4j;
import com.gym.fit_power.model.Gym;
import com.gym.fit_power.dto.GymDTO;
import com.gym.fit_power.service.GymService;
import org.springframework.stereotype.Service;
import com.gym.fit_power.repository.GymRepository;
import org.springframework.dao.DataAccessException;

@Slf4j
@Service
public class GymServiceImpl implements GymService {

    GymRepository repository;
    protected static final Logger logger = LoggerFactory.getLogger(GymServiceImpl.class);

    public GymServiceImpl(GymRepository gymRepository) {
        repository = gymRepository;
    }

    @Override
    public GymDTO create(GymDTO gymDTO) throws DataAccessException {
        Gym newGym = toEntity(gymDTO);
        try {
            log.info("Save the new gym at the address {}", newGym.getAddress());
            if (Boolean.TRUE.equals(verifyAddress(newGym.getAddress()))) {
                throw new Exception("no se admite dirección repetida");//TODO manejo de excepciones
            }
            newGym.setEnabled(true);
            return toDTO(repository.save(newGym));
        } catch (Exception e) {
            log.error("The gym could not be saved. Error:", e);
        }
        return null;
    }

    private Gym readOne(String address) throws DataAccessException {
        Gym result = new Gym();
        try {
            log.info("Searching the gym with address {} in the database", address);
            result = repository.findByAddress(address);
            if (result == null) {
                throw new Exception("no existe el gimnasio solicitado.");//TODO manejo de excepciones
            }
        } catch (Exception e) {
            log.error("An error has occurred. Error: ", e);
        }
        return result;
    }


    public GymDTO readByAddress(String address) throws DataAccessException {
        try {
            log.info("Searching the gym at the address {}", address);
            return toDTO(repository.findByAddress(address));
        } catch (Exception e) {
            log.error("An error has occurred while searching the gym. Error: ", e);
        }
        return null;
    }

    @Override
    public List<GymDTO> readAll() throws DataAccessException {
        try {
            List<GymDTO> response = new ArrayList<>();
            log.info("Searching all gyms on the database");
            for (Gym gym : repository.findAll()) {
                response.add(toDTO(gym));
            }
            return response;
        } catch (Exception e) {
            log.error("There are no gyms in the database. Error: ",e);
        }
        return new ArrayList<>();
    }

    @Override
    public GymDTO update(String cuit, GymDTO gymDTO) throws DataAccessException {
        Gym oldGym = readOne(cuit);
        try {
            log.info("Updating the gym in the address {}", cuit);
            Gym newGym = toEntity(gymDTO);
            if (Boolean.TRUE.equals(verifyAddress(newGym.getAddress())) &&
                    !oldGym.getAddress().equals(newGym.getAddress())) {
                throw new Exception("no se admite dirección repetida");//TODO manejo de excepciones
            }
            oldGym.setDomain(newGym.getDomain());
            oldGym.setMail(newGym.getMail());
            oldGym.setPhone(newGym.getPhone());
            return toDTO(repository.save(oldGym));
        } catch (Exception e) {
            log.error("The gym could not be updated. Error:", e);
        }
        return null;
    }

    @Override
    public GymDTO disable(String cuit) throws DataAccessException {
        Gym entity = readOne(cuit);
        try {
            log.info("Disabling the gym in the address {}", cuit);
            entity.setEnabled(false);
            return toDTO(repository.save(entity));
        } catch (Exception e) {
            log.error("The gym could not be disabled. Error:", e);
        }
        return null;
    }

    @Override
    public GymDTO enable(String cuit) throws DataAccessException {
        Gym entity = readOne(cuit);
        try {
            log.info("Enabling the gym in the address {}", cuit);
            entity.setEnabled(true);
            return toDTO(repository.save(entity));
        } catch (Exception e) {
            log.error("The gym could not be enabled. Error:", e);
        }
        return null;
    }

    @Override
    public Gym toEntity(GymDTO dto) {
        Gym entity = new Gym();
        entity.setDomain(dto.getDomain());
        entity.setAddress(dto.getAddress());
        entity.setMail(dto.getMail());
        entity.setPhone(dto.getPhone());
        return entity;
    }

    @Override
    public GymDTO toDTO(Gym gym) {
        GymDTO entity = new GymDTO();
        entity.setId(gym.getId());
        entity.setDomain(gym.getDomain());
        entity.setAddress(gym.getAddress());
        entity.setMail(gym.getMail());
        entity.setPhone(gym.getPhone());
        return entity;
    }

    private Boolean verifyAddress(String address) {
        for (Gym gym : repository.findAll()) {
            if (gym.getAddress().equals(address)) {
                return true;
            }
        }
        return false;
    }

}
