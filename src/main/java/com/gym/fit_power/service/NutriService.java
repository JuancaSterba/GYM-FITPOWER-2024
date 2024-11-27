package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.RequestNutri;
import com.gym.fit_power.dto.response.ResponseNutri;

import java.util.List;

public interface NutriService {

    ResponseNutri create (RequestNutri requestNutri);
    ResponseNutri readOne (Long id);
    List<ResponseNutri> readAll ();
    ResponseNutri update (Long id , RequestNutri requestNutri);
    ResponseNutri disable (Long id);
    ResponseNutri enable (Long id );
    ResponseNutri findByCuit ( String cuit );


}
