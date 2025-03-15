package com.gym.fit_power.service;

import java.util.List;
import org.springframework.dao.DataAccessException;

public interface ServiceCRUD<REQUEST, RESPONSE> {

    RESPONSE create(REQUEST request) throws DataAccessException;

    List<RESPONSE> readAll() throws DataAccessException;

    RESPONSE update(String cuit, REQUEST request) throws DataAccessException;

    RESPONSE disable(String cuit) throws DataAccessException;

    RESPONSE enable(String cuit) throws DataAccessException;

}
