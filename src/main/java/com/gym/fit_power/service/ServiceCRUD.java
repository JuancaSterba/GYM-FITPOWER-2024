package com.gym.fit_power.service;

import java.util.List;
import org.springframework.dao.DataAccessException;

public interface ServiceCRUD<REQUEST, RESPONSE> {

    RESPONSE create(REQUEST request) throws DataAccessException;

    RESPONSE readOne(Long id) throws DataAccessException;

    List<RESPONSE> readAll() throws DataAccessException;

    RESPONSE update(REQUEST request) throws DataAccessException;

    RESPONSE disable(REQUEST request) throws DataAccessException;

    RESPONSE enable(REQUEST request) throws DataAccessException;

}
