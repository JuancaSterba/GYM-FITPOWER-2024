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
        Gym newGym = toEntity(gymDTO);
        try {
            newInfoLog("Save the new gym: " + newGym.getDomain());
            newGym.setEnabled(true);
            return toDTO(repository.save(newGym));
        } catch (Exception e) {
            newErrorLog(gymCouldNotBE(newGym.getDomain()) + "saved. Error:", e);
        }
        return null;
    }

    @Override
    public GymDTO readOne(Long id) throws DataAccessException {
        try {
            newInfoLog("Searching the gym with id: " + id);
            return toDTO(repository.findById(id).orElseThrow());
        } catch (Exception e) {
            newErrorLog(SEARCH_ERROR, e);
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
            newErrorLog(SEARCH_ERROR, e);
        }
        return new ArrayList<>();
    }

    @Override
    public GymDTO update(Long id, GymDTO gymDTO) throws DataAccessException {
        Gym oldGym = toEntity(readOne(id));
        try {
            newInfoLog("Updating the gym " + oldGym.getDomain());
            Gym newGym = toEntity(gymDTO);
            oldGym.setDomain(newGym.getDomain());
            oldGym.setAddress(newGym.getAddress());
            oldGym.setMail(newGym.getMail());
            oldGym.setPhone(newGym.getPhone());
            return toDTO(repository.save(oldGym));
        } catch (Exception e) {
            newErrorLog(gymCouldNotBE(gymDTO.getCode()) + "updated. Error:", e);
        }
        return null;
    }

    @Override
    public GymDTO disable(Long id) throws DataAccessException {
        Gym entity = toEntity(readOne(id));
        try {
            newInfoLog("Disabling the gym " + entity.getDomain());
            entity.setEnabled(false);
            return toDTO(repository.save(entity));
        } catch (Exception e) {
            newErrorLog(gymCouldNotBE(entity.getDomain()) + "disabled. Error:", e);
        }
        return null;
    }

    @Override
    public GymDTO enable(Long id) throws DataAccessException {
        Gym entity = toEntity(readOne(id));
        try {
            newInfoLog("Enabling the gym " + entity.getDomain());
            entity.setEnabled(true);
            return toDTO(repository.save(entity));
        } catch (Exception e) {
            newErrorLog(gymCouldNotBE(entity.getDomain()) + "enabled. Error:", e);
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
        entity.setEnabled(dto.getEnabled());
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
        entity.setEnabled(gym.getEnabled());
        return entity;
    }

    private String gymCouldNotBE(String code){
        return "The gym " + code + " could not be";
    }

    private void newInfoLog(String description) {
        logger.info(SERVICE, description);
    }

    private void newErrorLog(String description, Exception e) {
        logger.error(SERVICE + " {}", description, e.getMessage());
    }

}
