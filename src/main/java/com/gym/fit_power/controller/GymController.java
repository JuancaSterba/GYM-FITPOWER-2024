package com.gym.fit_power.controller;

import java.net.URI;
import java.util.List;
import org.slf4j.Logger;
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
        GymDTO response = service.create(request);
        newInfoLog(request.getDomain() + " is created");
        return ResponseEntity.ok().headers(newHeader("CREATED", SUCCESSFUL)).
                location(new URI("/api/gyms/" + response.getAddress())).body(response);
    }

    @GetMapping(value = ADDRESS)
    public ResponseEntity<GymDTO> readOne(@PathVariable(value = "address") String address) {
        newInfoLog("Get a gym at the address: " + address);
        GymDTO response = service.readByAddress(address);
        correctSearch();
        return ResponseEntity.ok().headers(newHeader("FOUND", SUCCESSFUL)).body(response);
    }

    @GetMapping(value = "")
    public ResponseEntity<List<GymDTO>> readAll() {
        newInfoLog("Get all gyms");
        List<GymDTO> response = service.readAll();
        correctSearch();
        return ResponseEntity.ok().headers(newHeader("FOUND", SUCCESSFUL)).body(response);
    }

    @PutMapping(value = ADDRESS)
    public ResponseEntity<GymDTO> update(@PathVariable(value = "address") String address, @RequestBody GymDTO newGym) {
        newInfoLog("Update gym with address: " + address);
        GymDTO response = service.update(service.readByAddress(address).getId(), newGym);
        newInfoLog(response.getDomain() + " is updated");
        return ResponseEntity.ok().headers(newHeader("UPDATED", SUCCESSFUL)).body(response);
    }

    @DeleteMapping(value = ADDRESS)
    public ResponseEntity<GymDTO> disable(@PathVariable(value = "address") String address) {
        GymDTO gym = service.readByAddress(address);
        newInfoLog("Disabling gym " + gym.getDomain());
        GymDTO response = service.disable(gym.getId());
        newInfoLog("Gym disabled");
        return ResponseEntity.ok().headers(newHeader("DISABLED", SUCCESSFUL)).body(response);
    }

    @PatchMapping(value = ADDRESS)
    public ResponseEntity<GymDTO> enable(@PathVariable(value = "address") String address) {
        GymDTO dto = service.readByAddress(address);
        newInfoLog("Enabling gym with address: " + address);
        GymDTO response = service.enable(dto.getId());
        newInfoLog("Gym enabled");
        return ResponseEntity.ok().headers(newHeader("ENABLED", SUCCESSFUL)).body(response);
    }

    private void newInfoLog(String description) {
        logger.info(CONTROLLER, description);
    }

    private void correctSearch() {
        logger.info(SEARCH_CORRECT);
    }

    private HttpHeaders newHeader(String headerName, String description) {
        HttpHeaders headers = new HttpHeaders();
        headers.add(headerName, description);
        return headers;
    }

}
