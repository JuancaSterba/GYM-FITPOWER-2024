package com.gym.fit_power.controller;

import java.net.URI;
import java.util.List;

import org.slf4j.Logger;

import java.util.ArrayList;

import org.slf4j.LoggerFactory;
import lombok.extern.slf4j.Slf4j;

import java.net.URISyntaxException;

import com.gym.fit_power.dto.GymDTO;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import com.gym.fit_power.service.impl.GymServiceImpl;

import static com.gym.fit_power.constant.GymConstants.*;

@Slf4j
@RestController
@RequestMapping(GymController.RESOURCE)
public class GymController {

    private final GymServiceImpl service;
    public static final String ADDRESS = "/{address}";
    public static final String RESOURCE = "/api/gyms";
    private static final Logger logger = LoggerFactory.getLogger(GymController.class);

    public GymController(GymServiceImpl gymService) {
        service = gymService;
    }

    @PostMapping(value = "")
    public ResponseEntity<GymDTO> create(@RequestBody GymDTO request) throws URISyntaxException {
        newInfoLog("Creating new gym: " + request);
        GymDTO response;
        if (service.readByAddress(request.getAddress()) != null) {
            newErrorLog("creating", genericDescription(request.getDomain()) + " already exist");
            return ResponseEntity.badRequest()
                    .headers(newHeader("CREATE_ERROR", genericDescription(request.getDomain())
                            + "already exist"))
                    .body(request);
        }
        response = service.create(request);
        URI uri = new URI("/api/gyms/" + response.getAddress());
        newInfoLog(request.getDomain() + " is created");
        return ResponseEntity.ok().headers(newHeader("CREATED", SUCCESSFUL)).location(uri).body(response);
    }

    @GetMapping(value = ADDRESS)
    public ResponseEntity<GymDTO> readOne(@PathVariable(value = "address") String address) {
        newInfoLog("Get a gym at the address: " + address);
        GymDTO response = service.readByAddress(address);
        if (response == null) {
            errorSearch(notFoundDescription(address));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(address))).body(null);
        }
        correctSearch();
        return ResponseEntity.ok().headers(newHeader("FOUND", SUCCESSFUL)).body(response);
    }

    @GetMapping(value = "")
    public ResponseEntity<List<GymDTO>> readAll() {
        newInfoLog("Get all gyms");
        List<GymDTO> response = new ArrayList<>(service.readAll());
        if (response.isEmpty()) {
            errorSearch("There are no gyms in the database");
            return ResponseEntity.badRequest()
                    .headers(newHeader(ERR404, "There are no gyms in the database")).body(null);
        }
        correctSearch();
        return ResponseEntity.ok().headers(newHeader("FOUND", SUCCESSFUL)).body(response);
    }

    @PutMapping(value = ADDRESS)
    public ResponseEntity<GymDTO> update(@PathVariable(value = "address") String address, @RequestBody GymDTO newGym) {
        newInfoLog("Update gym with address: " + address);
        GymDTO oldGym = service.readByAddress(address);
        if (oldGym == null) {
            newErrorLog("updating", notFoundDescription(address));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(address))).body(null);
        }
        GymDTO response = service.update(oldGym.getId(), newGym);
        newInfoLog(oldGym.getDomain() + " is updated");
        return ResponseEntity.ok().headers(newHeader("UPDATED", SUCCESSFUL)).body(response);
    }

    @DeleteMapping(value = ADDRESS)
    public ResponseEntity<GymDTO> disable(@PathVariable(value = "address") String address) {
        GymDTO dto = service.readByAddress(address);
        newInfoLog("Disabling gym " + dto.getDomain());
        if (service.readByAddress(address) == null) {
            newErrorLog("disabling", notFoundDescription(address));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(address))).body(null);
        }
        if (Boolean.FALSE.equals(dto.getEnabled())) {
            newErrorLog("disabling", genericDescription(address) + "is already disabled");
            return ResponseEntity.badRequest()
                    .headers(newHeader("DISABLE_ERROR", genericDescription(address)
                            + "is already disabled"))
                    .body(dto);
        }
        GymDTO response = service.disable(dto.getId());
        newInfoLog("Gym disabled");
        return ResponseEntity.ok().headers(newHeader("DISABLED", SUCCESSFUL)).body(response);
    }

    @PatchMapping(value = ADDRESS)
    public ResponseEntity<GymDTO> enable(@PathVariable(value = "address") String address) {
        GymDTO dto = service.readByAddress(address);
        newInfoLog("Enabling gym with address: " + address);
        if (dto == null) {
            newErrorLog("enabling", notFoundDescription(address));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(address))).body(null);
        } else if (Boolean.TRUE.equals(dto.getEnabled())) {
            newErrorLog("enabling", genericDescription(address) + "is already enabled");
            return ResponseEntity.badRequest()
                    .headers(newHeader("ENABLE_ERROR", genericDescription(address)
                            + "is already enabled"))
                    .body(dto);
        }
        GymDTO response = service.enable(dto.getId());
        newInfoLog("Gym enabled");
        return ResponseEntity.ok().headers(newHeader("ENABLED", SUCCESSFUL)).body(response);
    }

    private void newInfoLog(String description) {
        logger.info(CONTROLLER, description);
    }

    private void newErrorLog(String task, String description) {
        logger.error(ERROR_WHILE, task, description);
    }

    private void correctSearch() {
        logger.info(SEARCH_CORRECT);
    }

    private void errorSearch(String error) {
        logger.error(SEARCH_ERROR, error);
    }

    private HttpHeaders newHeader(String headerName, String description) {
        HttpHeaders headers = new HttpHeaders();
        headers.add(headerName, description);
        return headers;
    }

    private String genericDescription(String domain) {
        return "the gym" + domain + " ";
    }

    private String notFoundDescription(String code) {
        return genericDescription(code) + "does not exist";
    }

}
