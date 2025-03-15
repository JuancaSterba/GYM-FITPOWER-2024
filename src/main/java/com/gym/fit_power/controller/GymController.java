package com.gym.fit_power.controller;

import java.net.URI;
import java.util.List;
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
@RequestMapping("/api/V1/gyms")
public class GymController {

    private final GymServiceImpl service;

    public GymController(GymServiceImpl gymService) {
        service = gymService;
    }

    @PostMapping
    public ResponseEntity<GymDTO> create(@RequestBody GymDTO request) throws URISyntaxException {
        log.info("Creating new gym: {}", request);
        GymDTO response = service.create(request);
        log.info("{} is created", request.getDomain());
        return ResponseEntity.ok().headers(newHeader("CREATED")).
                location(new URI("/api/v1/gyms/" + response.getAddress())).body(response);
    }

    @GetMapping(value = "/{address}")
    public ResponseEntity<GymDTO> readOne(@PathVariable(value = "address") String address) {
        log.info("Get a gym at the address: {}", address);
        GymDTO response = service.readByAddress(address);
        log.info("The gym was found correctly");
        return ResponseEntity.ok().headers(newHeader("FOUND")).body(response);
    }

    @GetMapping
    public ResponseEntity<List<GymDTO>> readAll() {
        log.info("Get all gyms");
        List<GymDTO> response = service.readAll();
        log.info("The gyms were found correctly");
        return ResponseEntity.ok().headers(newHeader("FOUND")).body(response);
    }

    @PutMapping(value = "/{address}")
    public ResponseEntity<GymDTO> update(@PathVariable(value = "address") String address, @RequestBody GymDTO newGym) {
        log.info("Update gym with address: {}", address);
        GymDTO response = service.update(address, newGym);
        log.info("{} is updated", response.getDomain());
        return ResponseEntity.ok().headers(newHeader("UPDATED")).body(response);
    }

    @DeleteMapping(value = "/{address}")
    public ResponseEntity<GymDTO> disable(@PathVariable(value = "address") String address) {
        log.info("Disabling gym in the address {}", address);
        GymDTO response = service.disable(address);
        log.info("Gym disabled");
        return ResponseEntity.ok().headers(newHeader("DISABLED")).body(response);
    }

    @PatchMapping(value = "/{address}")
    public ResponseEntity<GymDTO> enable(@PathVariable(value = "address") String address) {
        log.info("Enabling gym with address: {}", address);
        GymDTO response = service.enable(address);
        log.info("Gym enabled");
        return ResponseEntity.ok().headers(newHeader("ENABLED")).body(response);
    }

    private HttpHeaders newHeader(String headerName) {
        HttpHeaders headers = new HttpHeaders();
        headers.add(headerName, "the task was completed successfully");
        return headers;
    }

}
