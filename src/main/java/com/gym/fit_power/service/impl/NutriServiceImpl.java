package com.gym.fit_power.service.impl;


import com.gym.fit_power.dto.request.RequestNutri;
import com.gym.fit_power.dto.response.ResponseNutri;
import com.gym.fit_power.exception.EntityNotFoundException;
import com.gym.fit_power.exception.EntitySaveException;
import com.gym.fit_power.model.Nutritionist;
import com.gym.fit_power.repository.NutriRepository;
import com.gym.fit_power.service.NutriService;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static com.gym.fit_power.constant.NutritinistConstants.*;


@Service
@Slf4j
public class NutriServiceImpl implements NutriService {
    private final NutriRepository nutriRepository;
    protected static final Logger logger = LoggerFactory.getLogger(NutriServiceImpl.class);


    public NutriServiceImpl(NutriRepository nutriRepository) {
        this.nutriRepository = nutriRepository;

    }


    @Override
    @Transactional
    public ResponseNutri create(RequestNutri requestNutri) {
        ResponseNutri response = null;
        try {
            response = toDTO(nutriRepository.save(toEntity(requestNutri)));
            logger.info(CREATE_SUCCESFULLY, "{}" + requestNutri.getName());
        } catch (Exception e) {
            throw new EntitySaveException("Failed to save entity");
        }
        return response;
    }

    @Override
    @Transactional
    public ResponseNutri readOne(Long id) {
        Optional<Nutritionist>nutriOptional = nutriRepository.findById(id);
        Nutritionist nutri = nutriOptional.orElseThrow(()->{
            String message = ERR404 + id;
            return new EntityNotFoundException(message);
        });
        return toDTO(nutri);
    }

    @Override
    @Transactional
    public List<ResponseNutri> readAll() {
        List<Nutritionist>nutris = nutriRepository.findAll();
        return nutris
                .stream()
                .map(this::toDTO)
                .toList();
    }

    @Override
    @Transactional
    public ResponseNutri update(Long id, RequestNutri requestNutri) {
        Nutritionist nutriUpdate;
        Optional<Nutritionist>nutriOptional = nutriRepository.findById(id);
        Nutritionist nutri = nutriOptional.orElseThrow(()->{
            String message = ERR404 + id;
            return new EntityNotFoundException(message);
        });

        nutriUpdate = toEntity(requestNutri);
        nutri.setName(nutriUpdate.getName());
        nutri.setLastname(nutriUpdate.getLastname());
        nutri.setPhone(nutriUpdate.getPhone());
        nutri.setEmail(nutriUpdate.getEmail());

        nutriRepository.save(nutri);
        logger.info("{} {}",CREATE_SUCCESFULLY,nutri.getName());

        return toDTO(nutriUpdate);
    }

    @Override
    @Transactional
    public ResponseNutri disable(Long id) {
        Optional<Nutritionist>nutritOptional = nutriRepository.findById(id);
        Nutritionist nutri = nutritOptional.orElseThrow(()->{
            String message = ERR404 + id;
            return new EntityNotFoundException(message);
        });
        nutri.setEnabled(false);
        nutriRepository.save(nutri);
        logger.info("{} {}",SUCCESSFUL,nutri.getName());
        return toDTO(nutri);
    }

    @Override
    public ResponseNutri enable(Long id) {
        Optional<Nutritionist>nutritOptional = nutriRepository.findById(id);
        Nutritionist nutri = nutritOptional.orElseThrow(()->{
            String message = ERR404 + id;
            return new EntityNotFoundException(message);
        });
        nutri.setEnabled(true);
        nutriRepository.save(nutri);
        logger.info("{} {}",SUCCESSFUL,nutri.getName());
        return toDTO(nutri);
    }

    @Override
    public ResponseNutri findByCuit(String cuit) {
        return null;
    }

    private Nutritionist toEntity(RequestNutri requestNutri) {
        Nutritionist nutri = new Nutritionist();
        nutri.setName(requestNutri.getName());
        nutri.setLastname(requestNutri.getLastname());
        nutri.setCuit(requestNutri.getCuit());
        nutri.setPhone(requestNutri.getPhone());
        nutri.setEmail(requestNutri.getEmail());
        nutri.setCreatedAt(LocalDate.now());
        return nutri;
    }

    private ResponseNutri toDTO(Nutritionist nutri) {
        ResponseNutri response = new ResponseNutri();
        response.setName(nutri.getName());
        response.setLastname(nutri.getLastname());
        response.setCuit(nutri.getCuit());
        response.setPhone(nutri.getPhone());
        response.setEmail(nutri.getEmail());
        response.setCreatedAt(nutri.getCreatedAt().toString());
        return response;

    }

}
