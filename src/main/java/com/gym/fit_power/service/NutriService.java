package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.RequestNutri;
import com.gym.fit_power.dto.response.ResponseNutri;

import java.util.List;

public interface NutriService {

    ResponseNutri create (RequestNutri requestNutri);
    ResponseNutri readOne (String cuit);
    List<ResponseNutri> readAll ();
    ResponseNutri update (String cuit);
    ResponseNutri disable (String cuit);
    ResponseNutri enable (String cuit);


}
