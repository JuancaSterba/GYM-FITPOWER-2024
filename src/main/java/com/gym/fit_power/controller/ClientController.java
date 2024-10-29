package com.gym.fit_power.controller;

import com.gym.fit_power.dto.ClientDTO;
import com.gym.fit_power.service.impl.ClientServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import static com.gym.fit_power.constant.ClientConstants.*;

@Slf4j
@RestController
@RequestMapping(ClientController.RESOURCE)
public class ClientController {

    private final ClientServiceImpl service;
    public static final String CUIT = "/{cuit}";
    public static final String CHANGE = CUIT + "/change";
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

    @GetMapping(value = "/{gymCode}")
    public ResponseEntity<List<ClientDTO>> readByGym(@PathVariable(value = "gymCode") String gymCode) {
        newInfoLog("Get all clients from a Gym " + gymCode);
        List<ClientDTO> response = new ArrayList<>(service.readByGym(gymCode));
        if (response.isEmpty()) {
            String where = "in the gym";
            errorSearch(noClients(where, gymCode));
            return ResponseEntity.badRequest()
                    .headers(newHeader(ERR404, noClients(where, gymCode))).body(null);
        }
        correctSearch();
        return ResponseEntity.ok().headers(newHeader(FOUND, SUCCESSFUL)).body(response);
    }

    @GetMapping(value = "/{trainerCuit}")
    public ResponseEntity<List<ClientDTO>> readByTrainer(@PathVariable(value = "trainerCuit") String trainerCuit) {
        newInfoLog("Get all clients of Trainer " + trainerCuit);
        List<ClientDTO> response = new ArrayList<>(service.readByGym(trainerCuit));
        if (response.isEmpty()) {
            String where = "under charge of Trainer";
            errorSearch(noClients(where, trainerCuit));
            return ResponseEntity.badRequest()
                    .headers(newHeader(ERR404, noClients(where, trainerCuit))).body(null);
        }
        correctSearch();
        return ResponseEntity.ok().headers(newHeader(FOUND, SUCCESSFUL)).body(response);
    }

    @GetMapping(value = "/{nutCuit}")
    public ResponseEntity<List<ClientDTO>> readByNutritionist(@PathVariable(value = "nutCuit") String nutCuit) {
        newInfoLog("Get all clients of Nutritionist " + nutCuit);
        List<ClientDTO> response = new ArrayList<>(service.readByGym(nutCuit));
        if (response.isEmpty()) {
            String where = "under charge of Nutritionist";
            errorSearch(noClients(where, nutCuit));
            return ResponseEntity.badRequest()
                    .headers(newHeader(ERR404, noClients(where, nutCuit))).body(null);
        }
        correctSearch();
        return ResponseEntity.ok().headers(newHeader(FOUND, SUCCESSFUL)).body(response);
    }

    @GetMapping(value = "")
    public ResponseEntity<List<ClientDTO>> readAll() {
        newInfoLog("Get all client");
        List<ClientDTO> response = new ArrayList<>(service.readAll());
        if (response.isEmpty()) {
            String where = "in the database";
            errorSearch(noClients(where, ""));
            return ResponseEntity.badRequest()
                    .headers(newHeader(ERR404, noClients(where, ""))).body(null);
        }
        correctSearch();
        return ResponseEntity.ok().headers(newHeader(FOUND, SUCCESSFUL)).body(response);
    }

    @PutMapping(value = CUIT + "/personal")
    public ResponseEntity<ClientDTO> update(@PathVariable(value = "cuit") String cuit, @RequestBody ClientDTO client) {
        newInfoLog("Update personal data of client with cuit: " + cuit);
        ClientDTO oldClient = service.readByCuit(cuit);
        if (oldClient == null) {
            errorUpdateLog(notFoundDescription(cuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(cuit))).body(null);
        }
        ClientDTO response = service.update(client);
        newInfoLog(updatedDescription(cuit));
        return ResponseEntity.ok().headers(newHeader("PERSONAL_UPDATED", SUCCESSFUL)).body(response);
    }

    @PutMapping(value = CUIT + "/physics")
    public ResponseEntity<ClientDTO> updatePhysics(@PathVariable(value = "cuit") String cuit,
                                                   @RequestBody ClientDTO client) {
        newInfoLog("Update physic data of client with cuit: " + cuit);
        ClientDTO oldClient = service.readByCuit(cuit);
        if (oldClient == null) {
            errorUpdateLog(notFoundDescription(cuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(cuit))).body(null);
        }
        ClientDTO response = service.update(client);
        newInfoLog(updatedDescription(cuit));
        return ResponseEntity.ok().headers(newHeader("PHYSICS_UPDATED", SUCCESSFUL)).body(response);
    }

    @PutMapping(value = CUIT + "/allergies")
    public ResponseEntity<ClientDTO> updateAllergies(@PathVariable(value = "cuit") String cuit,
                                                     @RequestBody ClientDTO client) {
        newInfoLog("Update allergies of client with cuit: " + cuit);
        ClientDTO oldClient = service.readByCuit(cuit);
        if (oldClient == null) {
            errorUpdateLog(notFoundDescription(cuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(cuit))).body(null);
        }
        ClientDTO response = service.update(client);
        newInfoLog(updatedDescription(cuit));
        return ResponseEntity.ok().headers(newHeader("ALLERGIES_UPDATED", SUCCESSFUL)).body(response);
    }

    @PutMapping(value = CHANGE + "/{gymCode}")
    public ResponseEntity<ClientDTO> changeGym(@PathVariable(value = "cuit") String clientCuit,
                                               @PathVariable(value = "gymCode") String gymCode) {
        newInfoLog("Change the gym of the client " + clientCuit);
        ClientDTO oldClient = service.readByCuit(clientCuit);
        if (oldClient == null) {
            newErrorLog("changing the gym", notFoundDescription(clientCuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(clientCuit))).body(null);
        }
        ClientDTO response = service.changeGym(oldClient, gymCode);
        changedInfoLog("gym", clientCuit);
        return ResponseEntity.ok().headers(newHeader("GYM_CHANGED", SUCCESSFUL)).body(response);
    }

    @PutMapping(value = CUIT + "/{trainerCuit}")
    public ResponseEntity<ClientDTO> assignTrainer(@PathVariable(value = "cuit") String clientCuit,
                                                   @PathVariable(value = "trainerCuit") String trainerCuit) {
        newInfoLog("Assign trainer " + trainerCuit + " to the client " + clientCuit);
        ClientDTO oldClient = service.readByCuit(clientCuit);
        if (oldClient == null) {
            newErrorLog("assign a trainer", notFoundDescription(clientCuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(clientCuit))).body(null);
        }
        ClientDTO response = service.assignTrainer(oldClient, trainerCuit);
        newInfoLog("Trainer " + trainerCuit + " has been successfully assigned to the client " + clientCuit);
        return ResponseEntity.ok().headers(newHeader("TRAINER_ASSIGNED", SUCCESSFUL)).body(response);
    }

    @PutMapping(value = CHANGE + "/{trainerCuit}")
    public ResponseEntity<ClientDTO> changeTrainer(@PathVariable(value = "cuit") String clientCuit,
                                                   @PathVariable(value = "trainerCuit") String trainerCuit) {
        newInfoLog("Change the trainer of the client " + clientCuit);
        ClientDTO oldClient = service.readByCuit(clientCuit);
        if (oldClient == null) {
            newErrorLog("changing the trainer", notFoundDescription(clientCuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(clientCuit))).body(null);
        }
        ClientDTO response = service.changeTrainer(oldClient, trainerCuit);
        changedInfoLog("trainer", clientCuit);
        return ResponseEntity.ok().headers(newHeader("TRAINER_CHANGED", SUCCESSFUL)).body(response);
    }

    @DeleteMapping(value = CUIT + "/trainer")
    public ResponseEntity<ClientDTO> resignTrainer(@PathVariable(value = "cuit") String cuit) {
        newInfoLog("Unassign the trainer of the client " + cuit);
        ClientDTO client = service.readByCuit(cuit);
        if (client == null) {
            newErrorLog("resign the trainer", notFoundDescription(cuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(cuit))).body(null);
        }
        ClientDTO response = service.resignTrainer(client);
        resignedInfoLog("trainer", cuit);
        return ResponseEntity.ok().headers(newHeader("TRAINER_RESIGNED", SUCCESSFUL)).body(response);
    }

    @PutMapping(value = CUIT + "/{nutritionistCuit}")
    public ResponseEntity<ClientDTO> assignNutritionist(@PathVariable(value = "cuit") String clientCuit,
                                                        @PathVariable(value = "nutritionistCuit") String nutCuit) {
        newInfoLog("Assign nutritionist " + nutCuit + " to the client " + clientCuit);
        ClientDTO oldClient = service.readByCuit(clientCuit);
        if (oldClient == null) {
            newErrorLog("assign a nutritionist", notFoundDescription(clientCuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(clientCuit))).body(null);
        }
        ClientDTO response = service.assignNutritionist(oldClient, nutCuit);
        newInfoLog("Nutritionist " + nutCuit + " has been successfully assigned to the client " + clientCuit);
        return ResponseEntity.ok().headers(newHeader("NUTRITIONIST_ASSIGNED", SUCCESSFUL)).body(response);
    }

    @PutMapping(value = CHANGE + "/{nutritionistCuit}")
    public ResponseEntity<ClientDTO> changeNutritionist(@PathVariable(value = "cuit") String clientCuit,
                                                        @PathVariable(value = "nutritionistCuit") String nutCuit) {
        newInfoLog("Change the nutritionist of the client " + clientCuit);
        ClientDTO oldClient = service.readByCuit(clientCuit);
        if (oldClient == null) {
            newErrorLog("changing the nutritionist", notFoundDescription(clientCuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(clientCuit))).body(null);
        }
        ClientDTO response = service.changeNutritionist(oldClient, nutCuit);
        changedInfoLog("nutritionist", clientCuit);
        return ResponseEntity.ok().headers(newHeader("NUTRITIONIST_CHANGED", SUCCESSFUL)).body(response);
    }

    @DeleteMapping(value = CUIT + "/nutritionist")
    public ResponseEntity<ClientDTO> resignNutritionist(@PathVariable(value = "cuit") String cuit) {
        newInfoLog("Unassign the nutritionist of the client " + cuit);
        ClientDTO client = service.readByCuit(cuit);
        if (client == null) {
            newErrorLog("resign the nutritionist", notFoundDescription(cuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(cuit))).body(null);
        }
        ClientDTO response = service.resignNutritionist(client);
        resignedInfoLog("nutritionist", cuit);
        return ResponseEntity.ok().headers(newHeader("NUTRITIONIST_RESIGNED", SUCCESSFUL)).body(response);
    }

    @DeleteMapping(value = CUIT)
    public ResponseEntity<ClientDTO> disable(@PathVariable(value = "cuit") String cuit) {
        newInfoLog("Disabling client with cuit: " + cuit);
        ClientDTO dto = service.readByCuit(cuit);
        if (dto == null) {
            newErrorLog("disabling", notFoundDescription(cuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(cuit))).body(null);
        } else if (Boolean.FALSE.equals(dto.getEnabled())) {
            newErrorLog("disabling", genericDescription(cuit) + "is already disabled");
            return ResponseEntity.badRequest()
                    .headers(newHeader("DISABLE_ERROR", genericDescription(cuit)
                            + "is already disabled"))
                    .body(dto);
        }
        ClientDTO response = service.disable(dto);
        newInfoLog("Client disabled");
        return ResponseEntity.ok().headers(newHeader("DISABLED", SUCCESSFUL)).body(response);
    }

    @PatchMapping(value = CUIT)
    public ResponseEntity<ClientDTO> enable(@PathVariable(value = "cuit") String cuit) {
        newInfoLog("Enabling client with cuit: " + cuit);
        ClientDTO dto = service.readByCuit(cuit);
        if (dto == null) {
            newErrorLog("enabling", notFoundDescription(cuit));
            return ResponseEntity.badRequest().headers(newHeader(ERR404, notFoundDescription(cuit))).body(null);
        } else if (Boolean.TRUE.equals(dto.getEnabled())) {
            newErrorLog("enabling", genericDescription(cuit) + "is already enabled");
            return ResponseEntity.badRequest()
                    .headers(newHeader("ENABLE_ERROR", genericDescription(cuit)
                            + "is already enabled"))
                    .body(dto);
        }
        ClientDTO response = service.enable(dto);
        newInfoLog("Client enabled");
        return ResponseEntity.ok().headers(newHeader("ENABLED", SUCCESSFUL)).body(response);
    }

    private void changedInfoLog(String entity, String cuit) {
        newInfoLog("The " + entity + " of client " + cuit + " has changed successfully");
    }

    private void resignedInfoLog(String entity, String cuit) {
        newInfoLog("The " + entity + " of client " + cuit + " has resigned successfully");
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

    private String noClients(String where, String id) {
        return NO_CLIENTS + where + " " + id;
    }

}
