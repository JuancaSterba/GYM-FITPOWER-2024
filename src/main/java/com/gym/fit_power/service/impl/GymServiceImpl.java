package com.gym.fit_power.service.impl;

import java.util.List;
import org.slf4j.Logger;
import java.util.ArrayList;
import org.slf4j.LoggerFactory;
import lombok.extern.slf4j.Slf4j;
import com.gym.fit_power.model.Gym;
import java.util.GregorianCalendar;
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
        try {
            Gym newGym = toEntity(gymDTO);
            newInfoLog("Save the new gym: " + newGym.getCode());
            newGym.setEnabled(true);
            newGym.setCreatedAt(new GregorianCalendar());
            newGym.setUpdatedAt(new GregorianCalendar());
            return toDTO(repository.save(newGym));
        } catch (Exception e) {
            newErrorLog(gymCouldNotBE(gymDTO.getCode()) + "saved. Error:", e);
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

    public GymDTO readByCode(String code) throws DataAccessException {
        try {
            newInfoLog("Searching the gym with code: " + code);
            return toDTO(repository.findAll().stream()
                    .filter(gym -> gym.getCode().equals(code)).findFirst().orElseThrow());
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
            newErrorLog(gymCouldNotBE(gymDTO.getCode()) + "updated. Error:", e);
        }
        return null;
    }

    @Override
    public GymDTO disable(GymDTO gymDTO) throws DataAccessException {
        try {
            newInfoLog("Disabling the gym with code " + gymDTO.getCode());
            Gym entity = toEntity(gymDTO);
            entity.setEnabled(false);
            return toDTO(repository.save(entity));
        } catch (Exception e) {
            newErrorLog(gymCouldNotBE(gymDTO.getCode()) + "disabled. Error:", e);
        }
        return null;
    }

    @Override
    public GymDTO enable(GymDTO gymDTO) throws DataAccessException {
        try {
            newInfoLog("Enabling the gym with code " + gymDTO.getCode());
            Gym entity = toEntity(gymDTO);
            entity.setEnabled(true);
            return toDTO(repository.save(entity));
        } catch (Exception e) {
            newErrorLog(gymCouldNotBE(gymDTO.getCode()) + "enabled. Error:", e);
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
        entity.setEnabled(dto.getEnabled());
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
