package com.gym.fit_power.service;

import java.util.List;
import org.springframework.dao.DataAccessException;

public interface ServiceCRUD<REQUEST, RESPONSE> {

    RESPONSE create(REQUEST request) throws DataAccessException;

    RESPONSE readOne(Long id) throws DataAccessException;

    List<RESPONSE> readAll() throws DataAccessException;

    RESPONSE update(Long id, REQUEST request) throws DataAccessException;

    RESPONSE disable(Long id) throws DataAccessException;

    RESPONSE enable(Long id) throws DataAccessException;

}
