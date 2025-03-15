package com.gym.fit_power.controller;

import java.net.URI;
import java.util.List;

import java.util.Map;
import java.util.ArrayList;

import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.*;

import java.net.URISyntaxException;

import com.gym.fit_power.dto.NutriPlanDTO;
import com.gym.fit_power.dto.request.RoutineRequestDto;
import com.gym.fit_power.dto.request.TrainingDiaryRequestDto;
import com.gym.fit_power.dto.response.RoutineResponseDto;

import com.gym.fit_power.dto.ClientDTO;
import com.gym.fit_power.service.impl.*;
import com.gym.fit_power.dto.NutritionDiaryDTO;
import org.springframework.web.bind.annotation.*;

import static com.gym.fit_power.constant.ClientConstants.*;

@Slf4j
@RestController
@RequestMapping("/api/v1/clients")
public class ClientController {

    private final ClientServiceImpl clientService;
    private final RoutineServiceImpl routineService;
    private final NutriPlanServiceImpl nutritionPlanService;
    private final TrainingDiaryServiceImpl trainingDiaryService;
    private final NutritionDiaryServiceImpl nutritionDiaryService;

    public ClientController(ClientServiceImpl clientService,
                            NutriPlanServiceImpl nutritionPlanService,
                            NutritionDiaryServiceImpl nutritionDiaryService,
                            RoutineServiceImpl routineService,
                            TrainingDiaryServiceImpl trainingDiaryService) {
        this.clientService = clientService;
        this.routineService = routineService;
        this.nutritionPlanService = nutritionPlanService;
        this.nutritionDiaryService = nutritionDiaryService;
        this.trainingDiaryService = trainingDiaryService;
    }

    // <<<<<<<<<<<<<<<<<<< CLIENTS >>>>>>>>>>>>>>>>>>> //

    @PostMapping
    public ResponseEntity<ClientDTO> create(@RequestBody ClientDTO request) throws URISyntaxException {
        log.info("Creating new client: " + "{}", request);
        ClientDTO response = clientService.create(request);
        log.info("the client was created");
        return ResponseEntity.ok().headers(newHeader("CREATED")).
                location(new URI("/api/clients/" + response.getCuit())).body(response);
    }

    @GetMapping(value = "/{cuit}")
    public ResponseEntity<ClientDTO> readOne(@PathVariable(value = "cuit") String cuit) {
        log.info("Get client with cuit: " + "{}", cuit);
        ClientDTO response = clientService.readByCuit(cuit);
        log.info("Client was successfully found");
        return ResponseEntity.ok().headers(newHeader(FOUND)).body(response);
    }

    @GetMapping
    public ResponseEntity<List<ClientDTO>> readAll() {
        log.info("Get all clients");
        List<ClientDTO> response = new ArrayList<>(clientService.readAll());
        log.info("Clients were successfully found");
        return ResponseEntity.ok().headers(newHeader(FOUND)).body(response);
    }

    @PutMapping(value = "/{cuit}")
    public ResponseEntity<ClientDTO> update(@PathVariable(value = "cuit") String cuit, @RequestBody ClientDTO client) {
        log.info("Update personal data of client with cuit: " + "{}", cuit);
        ClientDTO response = clientService.update(cuit, client);
        log.info("the personal data is updated");
        return ResponseEntity.ok().headers(newHeader("UPDATED")).body(response);
    }

    @PutMapping(value = "/{cuit}/{gymCode}")
    public ResponseEntity<ClientDTO> changeGym(@PathVariable(value = "cuit") String clientCuit,
                                               @PathVariable(value = "gymCode") String gymCode) {
        log.info("Change the gym of the client " + "{}", clientCuit);
        ClientDTO response = clientService.changeGym(clientCuit, gymCode);
        log.info("The gym has changed successfully");
        return ResponseEntity.ok().headers(newHeader("GYM_CHANGED")).body(response);
    }

    @DeleteMapping(value = "/{cuit}")
    public ResponseEntity<ClientDTO> disable(@PathVariable(value = "cuit") String cuit) {
        log.info("Disabling client with cuit: " + "{}", cuit);
        ClientDTO response = clientService.disable(cuit);
        log.info("Client disabled");
        return ResponseEntity.ok().headers(newHeader("DISABLED")).body(response);
    }

    @PatchMapping(value = "/{cuit}")
    public ResponseEntity<ClientDTO> enable(@PathVariable(value = "cuit") String cuit) {
        log.info("Enabling client with cuit: " + "{}", cuit);
        ClientDTO response = clientService.enable(cuit);
        log.info("Client enabled");
        return ResponseEntity.ok().headers(newHeader("ENABLED")).body(response);
    }

    private HttpHeaders newHeader(String headerName) {
        HttpHeaders headers = new HttpHeaders();
        headers.add(headerName, SUCCESSFUL);
        return headers;
    }

    // <<<<<<<<<<<<<<<<<<< ROUTINES >>>>>>>>>>>>>>>>>>>

    @GetMapping("/{cuit}/routines")
    public ResponseEntity<List<RoutineResponseDto>> viewRoutines(@PathVariable(value = "cuit") String clientCuit) {
        return new ResponseEntity<>(routineService.findByClient(clientCuit), HttpStatus.OK);
    }

    @GetMapping("/{cuit}/routine")
    public ResponseEntity<RoutineResponseDto> viewActiveRoutine(@PathVariable(value = "cuit") String clientCuit) {
        return new ResponseEntity<>(routineService.findClientActiveRoutine(clientCuit), HttpStatus.OK);
    }

    @PostMapping("/{cuit}/routine")
    public ResponseEntity<RoutineResponseDto> createRoutine(@Valid @RequestBody RoutineRequestDto request, @PathVariable(value = "cuit") String clientCuit) {
        return new ResponseEntity<>(routineService.save(request, clientCuit), HttpStatus.CREATED);
    }

    // <<<<<<<<<<<<<<<< TRAINING-DIARY >>>>>>>>>>>>>>>> //

    @PostMapping("/{cuit}/routine/diary")
    public ResponseEntity<Object> addTrainingDiary(@Valid @RequestBody TrainingDiaryRequestDto request, @PathVariable(value = "cuit") String clientCuit) {
        trainingDiaryService.add(request, clientCuit);
        return new ResponseEntity<>(Map.of("message", "Diario de rutina agregado exitosamente para el cliente con CUIT " + clientCuit), HttpStatus.CREATED);
    }

/*

    @GetMapping("/{cuit}/routines/{id}/diary")
    public ResponseEntity<List<RoutineDiaryDTO>> viewRoutineDiary(@PathVariable(value = "cuit") String clientCuit,
                                                                      @PathVariable(value = "id") String id) {
        return new ResponseEntity<>(routineDiaryService.readByRoutine(id), HttpStatus.OK);
    }

    @GetMapping("/{cuit}/routines/active/diary")
    public ResponseEntity<List<RoutineDiaryDTO>> viewActiveRoutineDiary(@PathVariable(value = "cuit") String clientCuit) {
        return new ResponseEntity<>(routineDiaryService.readByClientActiveRoutine(clientCuit), HttpStatus.OK);
    }

*/


    @GetMapping("/{cuit}/nutrition_plans")
    public ResponseEntity<List<NutriPlanDTO>> viewNutritionPlans(@PathVariable(value = "cuit") String clientCuit) {
        return new ResponseEntity<>(nutritionPlanService.readByClient(clientCuit), HttpStatus.OK);
    }

    @GetMapping("/{cuit}/nutrition_plans/active")
    public ResponseEntity<NutriPlanDTO> viewActivePlan(@PathVariable(value = "cuit") String clientCuit) {
        return new ResponseEntity<>(nutritionPlanService.readPlanActiveByClient(clientCuit), HttpStatus.OK);
    }

    @GetMapping("/{cuit}/nutrition_plans/{id}")
    public ResponseEntity<NutriPlanDTO> viewOnePlan(@PathVariable(value = "cuit") String clientCuit,
                                                    @PathVariable(value = "id") Long id) {
        return new ResponseEntity<>(nutritionPlanService.readPlanByClient(clientCuit, id), HttpStatus.OK);
    }


    @PutMapping("/{cuit}/nutrition_plans/active/diary")
    public ResponseEntity<NutritionDiaryDTO> updateOrCreateNutritionDiary(
            @PathVariable(value = "cuit") String clientCuit,
            @RequestBody NutritionDiaryDTO request) {
        return new ResponseEntity<>(nutritionDiaryService.update(clientCuit, request), HttpStatus.OK);
    }

    @GetMapping("/{cuit}/nutrition_plans/{id}/diary")
    public ResponseEntity<List<NutritionDiaryDTO>> viewNutriPlanDiary(@PathVariable(value = "cuit") String clientCuit,
                                                                      @PathVariable(value = "id") Long id) {
        return new ResponseEntity<>(nutritionDiaryService.readByNutritionPlan(clientCuit, id), HttpStatus.OK);
    }

    @GetMapping("/{cuit}/nutrition_plans/active/diary")
    public ResponseEntity<List<NutritionDiaryDTO>> viewActivePlanDiary(@PathVariable(value = "cuit") String clientCuit) {
        return new ResponseEntity<>(nutritionDiaryService.readByClientActivePlan(clientCuit), HttpStatus.OK);
    }
  
}
