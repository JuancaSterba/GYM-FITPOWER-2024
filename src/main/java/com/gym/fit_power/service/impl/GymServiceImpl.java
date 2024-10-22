package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.GymDTO;
import com.gym.fit_power.model.Gym;
import com.gym.fit_power.repository.GymRepository;
import com.gym.fit_power.service.GymService;
import jakarta.persistence.Column;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.List;

import static com.gym.fit_power.constant.GymConstants.*;

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
        try {
            Gym newGym = toEntity(gymDTO);
            newInfoLog("Save the new gym: " + newGym.getCode());
            newGym.setActive(true);
            newGym.setCreatedAt(new GregorianCalendar());
            newGym.setUpdatedAt(new GregorianCalendar());
            return toDTO(repository.save(newGym));
        } catch (Exception e) {
            newErrorLog("Could not save the new gym " + gymDTO.getCode() + ". Error:", e);
        }
        return null;
    }

    @Override
    public GymDTO readOne(Long id) throws DataAccessException {
        try {
            newInfoLog("Searching the gym with id: " + id);
            return toDTO(repository.findById(id).orElseThrow());
        } catch (Exception e) {
            newErrorLog(ERROR_SEARCH, e);
        }
        return null;
    }

    public GymDTO readByCode(String code) throws DataAccessException {
        try {
            newInfoLog("Searching the gym with code: " + code);
            return toDTO(repository.findAll().stream()
                    .filter(gym -> gym.getCode().equals(code)).findFirst().orElseThrow());
        } catch (Exception e) {
            newErrorLog(ERROR_SEARCH, e);

        }
        return null;
    }

    @Override
    public List<GymDTO> readAll() throws DataAccessException {
        try {
            List<GymDTO> response = new ArrayList<>();
            newInfoLog("Searching all gyms on the database");
            for (Gym gym : repository.findAll()) {
                response.add(toDTO(gym));
            }
            return response;
        } catch (Exception e) {
            newErrorLog(ERROR_SEARCH, e);
        }
        return new ArrayList<>();
    }

    @Override
    public GymDTO update(GymDTO gymDTO) throws DataAccessException {
        try {
            newInfoLog("Updating the gym with code: " + gymDTO.getCode());
            Gym oldGym = toEntity(readByCode(gymDTO.getCode()));
            Gym newGym = toEntity(gymDTO);
            oldGym.setDomain(newGym.getDomain());
            oldGym.setAddress(newGym.getAddress());
            oldGym.setMail(newGym.getMail());
            oldGym.setPhone(newGym.getPhone());
            oldGym.setUpdatedAt(new GregorianCalendar());
            return toDTO(repository.save(oldGym));
        } catch (Exception e) {
            newErrorLog("The gym " + gymDTO.getCode() + " could not be updated. Error:", e);
        }
        return null;
    }

    @Override
    public GymDTO disable(GymDTO gymDTO) throws DataAccessException {
        try {
            newInfoLog("Disabling the gym with code " + gymDTO.getCode());
            Gym entity = toEntity(gymDTO);
            entity.setActive(false);
            return toDTO(repository.save(entity));
        } catch (Exception e) {
            newErrorLog("The gym " + gymDTO.getCode() + " could not be disabled. Error:", e);
        }
        return null;
    }

    @Override
    public GymDTO enable(GymDTO gymDTO) throws DataAccessException {
        try {
            newInfoLog("Enabling the gym with code " + gymDTO.getCode());
            Gym entity = toEntity(gymDTO);
            entity.setActive(true);
            return toDTO(repository.save(entity));
        } catch (Exception e) {
            newErrorLog("The gym " + gymDTO.getCode() + " could not be enabled. Error:", e);
        }
        return null;
    }

    @Override
    public Gym toEntity(GymDTO dto) {
        Gym entity = new Gym();
        entity.setCode(dto.getCode());
        entity.setDomain(dto.getDomain());
        entity.setAddress(dto.getAddress());
        entity.setMail(dto.getMail());
        entity.setPhone(dto.getPhone());
        entity.setActive(dto.getActive());
        return entity;
    }

    @Override
    public GymDTO toDTO(Gym gym) {
        GymDTO entity = new GymDTO();
        entity.setId(gym.getId());
        entity.setCode(gym.getCode());
        entity.setDomain(gym.getDomain());
        entity.setAddress(gym.getAddress());
        entity.setMail(gym.getMail());
        entity.setPhone(gym.getPhone());
        entity.setCreatedAt(gym.getCreatedAt().getTime().toString());
        entity.setUpdatedAt(gym.getUpdatedAt().getTime().toString());
        entity.setActive(gym.getActive());
        return entity;
    }

    private void newInfoLog(String description) {
        logger.info(SERVICE, description);
    }

    private void newErrorLog(String description, Exception e) {
        logger.error(SERVICE + " {}", description, e.getMessage());
    }

}
