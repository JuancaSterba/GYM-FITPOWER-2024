package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.RequestNutri;
import com.gym.fit_power.dto.response.ResponseNutri;

import java.util.List;

public interface NutriService {

    ResponseNutri craate (RequestNutri requestNutri);
    ResponseNutri readOne (String cuit);
    List<ResponseNutri> readAll ();
    ResponseNutri update (String code);
    ResponseNutri disable (String code);
    ResponseNutri enable (String code);


}
