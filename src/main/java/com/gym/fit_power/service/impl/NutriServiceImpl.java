package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.request.RequestNutri;
import com.gym.fit_power.dto.response.ResponseNutri;
import com.gym.fit_power.model.Nutritionist;
import com.gym.fit_power.repository.NutriRepository;
import com.gym.fit_power.service.NutriService;
import com.gym.fit_power.util.MyGregorianCalendarConverter;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.GregorianCalendar;
import java.util.List;
import java.util.stream.Collectors;

@Service
@Slf4j
public class NutriServiceImpl implements NutriService {
    private final NutriRepository nutriRepository;
    protected static final Logger logger = LoggerFactory.getLogger(NutriServiceImpl.class);
    private final MyGregorianCalendarConverter converter;

    public NutriServiceImpl(NutriRepository nutriRepository, MyGregorianCalendarConverter converter) {
        this.nutriRepository = nutriRepository;
        this.converter = new MyGregorianCalendarConverter();
    }


    @Override
    public ResponseNutri create(RequestNutri requestNutri) {
        Nutritionist nutritionist = toEntity(requestNutri);
        ResponseNutri responseNutri = toDTO(nutriRepository.save(nutritionist));
        return responseNutri;
    }

    @Override
    public ResponseNutri readOne(String cuit) {
        Nutritionist nutri = nutriRepository.findAll().stream()
                .filter(nutritionist -> nutritionist.getCuit().equals(cuit))
                .findFirst().orElse(null);
        return toDTO(nutri);
    }

    @Override
    public List<ResponseNutri> readAll() {
        return nutriRepository.findAll().stream()
                .map(this::toDTO)
                .collect(Collectors.toList());
    }

    @Override
    public ResponseNutri update(String cuit) {
        Nutritionist nutri = nutriRepository.findAll().stream()
                .filter(nutritionist -> nutritionist.getCuit().equals(cuit))
                .findFirst()
                .orElse(null);

        return null;
    }

    @Override
    public ResponseNutri disable(String code) {
        return null;
    }

    @Override
    public ResponseNutri enable(String code) {
        return null;
    }

    private Nutritionist toEntity(RequestNutri requestNutri) {
        Nutritionist nutri = new Nutritionist();
        nutri.setName(requestNutri.getName());
        nutri.setLastname(requestNutri.getLastname());
        nutri.setCuit(requestNutri.getCuit());
        nutri.setPhone(requestNutri.getPhone());
        nutri.setEmail(requestNutri.getEmail());
        nutri.setSpeciality(requestNutri.getSpeciality());
        String birthday = requestNutri.getBirthdate();
        GregorianCalendar birthday1 = converter.convertToEntityAttribute(birthday);
        nutri.setBirthdate(birthday1);
        return nutri;
    }

    private ResponseNutri toDTO(Nutritionist nutri) {
        ResponseNutri nutri1 = new ResponseNutri();
        nutri1.setName(nutri.getName());
        nutri1.setLastname(nutri.getLastname());
        nutri1.setCuit(nutri.getCuit());
        nutri1.setPhone(nutri.getPhone());
        nutri1.setEmail(nutri.getEmail());
        nutri1.setSpeciality(nutri.getSpeciality());
        nutri1.setBirthdate(nutri.getBirthdate().toString());
        GregorianCalendar craeteAt = nutri.getCreatedAt();
        nutri1.setCreatedAt(craeteAt.toString());
        GregorianCalendar updateAt = nutri.getUpdatedAt();
        nutri1.setUpdatedAt(updateAt.toString());

        return nutri1;
    }
}
