package com.gym.fit_power.controller;

import java.net.URI;
import java.util.List;
import org.slf4j.Logger;
import java.util.ArrayList;
import org.slf4j.LoggerFactory;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.*;
import java.net.URISyntaxException;
import com.gym.fit_power.dto.ClientDTO;
import org.springframework.web.bind.annotation.*;
import com.gym.fit_power.service.impl.ClientServiceImpl;

import static com.gym.fit_power.constant.ClientConstants.*;

@Slf4j
@RestController
@RequestMapping(ClientController.RESOURCE)
public class ClientController {

    private final ClientServiceImpl service;
    public static final String CUIT = "/{cuit}";
    public static final String RESOURCE = "/api/clients";
    private static final Logger logger = LoggerFactory.getLogger(ClientController.class);

    public ClientController(ClientServiceImpl clientService) {
        service = clientService;
    }

    @PostMapping(value = "")
    public ResponseEntity<ClientDTO> create(@RequestBody ClientDTO request) throws URISyntaxException {
        newInfoLog("Creating new client: " + request);
        ClientDTO response;
        if (service.readByCuit(request.getCuit()) != null) {
            newErrorLog("creating", genericDescription(request.getCuit()) + " already exist");
            return ResponseEntity.badRequest()
                    .headers(newHeader("CREATE_ERROR", genericDescription(request.getCuit())
                            + "already exist"))
                    .body(request);
        }
        response = service.create(request);
        URI uri = new URI("/api/clients/" + response.getCuit());
        newInfoLog(WITH_CUIT + request.getCuit() + " is created");
        return ResponseEntity.ok().headers(newHeader("CREATED", SUCCESSFUL)).location(uri).body(response);
    }

    @GetMapping(value = CUIT)
    public ResponseEntity<ClientDTO> readOne(@PathVariable(value = "cuit") String cuit) {
        newInfoLog("Get client with cuit: " + cuit);
        ClientDTO response = service.readByCuit(cuit);
        if (response == null) {
            errorSearch(notFoundDescription(cuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(cuit))).body(null);
        }
        correctSearch();
        return ResponseEntity.ok().headers(newHeader(FOUND, SUCCESSFUL)).body(response);
    }

    @GetMapping(value = "")
    public ResponseEntity<List<ClientDTO>> readAll() {
        newInfoLog("Get all client");
        List<ClientDTO> response = new ArrayList<>(service.readAll());
        if (response.isEmpty()) {
            String message = "there are no clients in the database";
            errorSearch(message);
            return ResponseEntity.badRequest()
                    .headers(newHeader(ERR404, message)).body(null);
        }
        correctSearch();
        return ResponseEntity.ok().headers(newHeader(FOUND, SUCCESSFUL)).body(response);
    }

    @PutMapping(value = CUIT)
    public ResponseEntity<ClientDTO> update(@PathVariable(value = "cuit") String cuit, @RequestBody ClientDTO client) {
        newInfoLog("Update personal data of client with cuit: " + cuit);
        ClientDTO oldClient = service.readByCuit(cuit);
        if (oldClient == null) {
            errorUpdateLog(notFoundDescription(cuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(cuit))).body(null);
        }
        ClientDTO response = service.update(oldClient.getId(), client);
        newInfoLog(updatedDescription(cuit));
        return ResponseEntity.ok().headers(newHeader("UPDATED", SUCCESSFUL)).body(response);
    }

    @PutMapping(value = CUIT + "/{gymCode}")
    public ResponseEntity<ClientDTO> changeGym(@PathVariable(value = "cuit") String clientCuit,
                                               @PathVariable(value = "gymCode") String gymCode) {
        newInfoLog("Change the gym of the client " + clientCuit);
        if (service.readByCuit(clientCuit) == null) {
            newErrorLog("changing the gym", notFoundDescription(clientCuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(clientCuit))).body(null);
        }
        ClientDTO response = service.changeGym(clientCuit, gymCode);
        newInfoLog("The gym of client " + clientCuit + " has changed successfully");
        return ResponseEntity.ok().headers(newHeader("GYM_CHANGED", SUCCESSFUL)).body(response);
    }

    @DeleteMapping(value = CUIT)
    public ResponseEntity<ClientDTO> disable(@PathVariable(value = "cuit") String cuit) {
        newInfoLog("Disabling client with cuit: " + cuit);
        ClientDTO client = service.readByCuit(cuit);
        if (client == null) {
            newErrorLog("disabling", notFoundDescription(cuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(cuit))).body(null);
        } else if (Boolean.FALSE.equals(client.getEnabled())) {
            newErrorLog("disabling", genericDescription(cuit) + "is already disabled");
            return ResponseEntity.badRequest()
                    .headers(newHeader("DISABLE_ERROR", genericDescription(cuit)
                            + "is already disabled"))
                    .body(client);
        }
        ClientDTO response = service.disable(client.getId());
        newInfoLog("Client disabled");
        return ResponseEntity.ok().headers(newHeader("DISABLED", SUCCESSFUL)).body(response);
    }

    @PatchMapping(value = CUIT)
    public ResponseEntity<ClientDTO> enable(@PathVariable(value = "cuit") String cuit) {
        newInfoLog("Enabling client with cuit: " + cuit);
        ClientDTO client = service.readByCuit(cuit);
        if (client == null) {
            newErrorLog("enabling", notFoundDescription(cuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(cuit))).body(null);
        } else if (Boolean.TRUE.equals(client.getEnabled())) {
            newErrorLog("enabling", genericDescription(cuit) + "is already enabled");
            return ResponseEntity.badRequest()
                    .headers(newHeader("ENABLE_ERROR", genericDescription(cuit)
                            + "is already enabled"))
                    .body(client);
        }
        ClientDTO response = service.enable(client.getId());
        newInfoLog("Client enabled");
        return ResponseEntity.ok().headers(newHeader("ENABLED", SUCCESSFUL)).body(response);
    }

    private void newInfoLog(String description) {
        logger.info(CONTROLLER, description);
    }

    private void errorUpdateLog(String description) {
        newErrorLog("updating", description);
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

    private String genericDescription(String cuit) {
        return WITH_CUIT + cuit + " ";
    }

    private String notFoundDescription(String cuit) {
        return genericDescription(cuit) + "does not exist";
    }

    private String updatedDescription(String cuit) {
        return genericDescription(cuit) + "is updated";
    }

}
