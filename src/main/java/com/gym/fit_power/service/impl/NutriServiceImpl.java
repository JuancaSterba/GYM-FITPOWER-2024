package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.request.RequestNutri;
import com.gym.fit_power.dto.response.ResponseNutri;
import com.gym.fit_power.service.NutriService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Slf4j
public class NutriServiceImpl implements NutriService {

    @Override
    public ResponseNutri craate(RequestNutri requestNutri) {
        return null;
    }

    @Override
    public ResponseNutri readOne(String cuit) {
        return null;
    }

    @Override
    public List<ResponseNutri> readAll() {
        return List.of();
    }

    @Override
    public ResponseNutri update(String code) {
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
}
