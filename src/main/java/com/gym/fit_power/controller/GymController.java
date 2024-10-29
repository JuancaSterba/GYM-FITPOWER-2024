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
    public static final String CODE = "/{code}";
    public static final String RESOURCE = "/api/gyms";
    private static final Logger logger = LoggerFactory.getLogger(GymController.class);

    public GymController(GymServiceImpl gymService) {
        service = gymService;
    }

    @PostMapping(value = "")
    public ResponseEntity<GymDTO> create(@RequestBody GymDTO request) throws URISyntaxException {
        newInfoLog("Creating new gym: " + request);
        GymDTO response;
        if (service.readByCode(request.getCode()) != null) {
            newErrorLog("creating", genericDescription(request.getCode()) + " already exist");
            return ResponseEntity.badRequest()
                    .headers(newHeader("CREATE_ERROR", genericDescription(request.getCode())
                            + "already exist"))
                    .body(request);
        }
        response = service.create(request);
        URI uri = new URI("/api/gyms/" + response.getCode());
        newInfoLog(WITH_CODE + request.getCode() + " is created");
        return ResponseEntity.ok().headers(newHeader("CREATED", SUCCESSFUL)).location(uri).body(response);
    }

    @GetMapping(value = CODE)
    public ResponseEntity<GymDTO> readOne(@PathVariable(value = "code") String code) {
        newInfoLog("Get gym with code: " + code);
        GymDTO response = service.readByCode(code);
        if (response == null) {
            errorSearch(notFoundDescription(code));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(code))).body(null);
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

    @PutMapping(value = CODE)
    public ResponseEntity<GymDTO> update(@PathVariable(value = "code") String code, @RequestBody GymDTO newGym) {
        newInfoLog("Update gym with code: " + code);
        GymDTO oldGym = service.readByCode(code);
        if (oldGym == null) {
            newErrorLog("updating", notFoundDescription(code));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(code))).body(null);
        }
        GymDTO response = service.update(newGym);
        newInfoLog(WITH_CODE + code + " is updated");
        return ResponseEntity.ok().headers(newHeader("UPDATED", SUCCESSFUL)).body(response);
    }

    @DeleteMapping(value = CODE)
    public ResponseEntity<GymDTO> disable(@PathVariable(value = "code") String code) {
        newInfoLog("Disabling gym with code: " + code);
        GymDTO dto = service.readByCode(code);
        if (dto == null) {
            newErrorLog("disabling", notFoundDescription(code));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(code))).body(null);
        } else if (Boolean.FALSE.equals(dto.getEnabled())) {
            newErrorLog("disabling", genericDescription(code) + "is already disabled");
            return ResponseEntity.badRequest()
                    .headers(newHeader("DISABLE_ERROR", genericDescription(code)
                            + "is already disabled"))
                    .body(dto);
        }
        GymDTO response = service.disable(dto);
        newInfoLog("Gym disabled");
        return ResponseEntity.ok().headers(newHeader("DISABLED", SUCCESSFUL)).body(response);
    }

    @PatchMapping(value = CODE)
    public ResponseEntity<GymDTO> enable(@PathVariable(value = "code") String code) {
        newInfoLog("Enabling gym with code: " + code);
        GymDTO dto = service.readByCode(code);
        if (dto == null) {
            newErrorLog("enabling", notFoundDescription(code));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(code))).body(null);
        } else if (Boolean.TRUE.equals(dto.getEnabled())) {
            newErrorLog("enabling", genericDescription(code) + "is already enabled");
            return ResponseEntity.badRequest()
                    .headers(newHeader("ENABLE_ERROR", genericDescription(code)
                            + "is already enabled"))
                    .body(dto);
        }
        GymDTO response = service.enable(dto);
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

    private String genericDescription(String code) {
        return WITH_CODE + code + " ";
    }

    private String notFoundDescription(String code) {
        return genericDescription(code) + "does not exist";
    }

}
