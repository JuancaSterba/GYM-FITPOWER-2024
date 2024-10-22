package com.gym.fit_power.controller;

import com.gym.fit_power.dto.request.TrainerRequestDto;
import com.gym.fit_power.dto.response.TrainerResponseDto;
import com.gym.fit_power.exception.DuplicatedTrainerException;
import com.gym.fit_power.exception.TrainerNotFoundException;
import com.gym.fit_power.exception.TrainerUpdateException;
import com.gym.fit_power.service.TrainerService;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;
import java.util.Map;

@RestController
@RequestMapping("api/v1/trainer")
@Validated
public class TrainerController {

    private final TrainerService trainerService;

    public TrainerController(TrainerService trainerService) {
        this.trainerService = trainerService;
    }

    @GetMapping
    public ResponseEntity<?> findAll() {
        return ResponseEntity.ok(trainerService.findAll());
    }

    @GetMapping("/{dni}")
    public ResponseEntity<?> findByDni(@PathVariable String dni) {
        try {
            TrainerResponseDto trainer = trainerService.findByDni(dni);
            return ResponseEntity.ok(trainer);
        } catch (TrainerNotFoundException e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(Collections.singletonMap("error", "No se encontró el entrenador con DNI " + dni));
        }
    }

    @PostMapping
    public ResponseEntity<Map<String, String>> save(@Valid @RequestBody TrainerRequestDto trainerRequestDto) {
        try {
            trainerService.save(trainerRequestDto);
            Map<String, String> response = Collections.singletonMap("message", "Entrenador creado con éxito");
            return ResponseEntity.ok(response);
        } catch (DuplicatedTrainerException e) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(Collections.singletonMap("error", "Ya existe un entrenador con DNI " + trainerRequestDto.getDni()));
        }
    }

    @PutMapping("/{dni}")
    public ResponseEntity<?> update(@PathVariable String dni, @Valid @RequestBody TrainerRequestDto trainerRequestDto) {
        try {
            trainerService.update(dni, trainerRequestDto);
            return ResponseEntity.ok(Collections.singletonMap("message", "Entrenador actualizado con éxito"));
        } catch (TrainerUpdateException e) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(Collections.singletonMap("error", "No se puede actualizar el DNI del entrenador"));
        } catch (TrainerNotFoundException e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(Collections.singletonMap("error", "No se encontró el entrenador con DNI " + dni));
        }
    }

    @DeleteMapping("/{dni}")
    public ResponseEntity<?> delete(@PathVariable String dni) {
        try {
            trainerService.delete(dni);
            return ResponseEntity.ok(Collections.singletonMap("message", "Entrenador eliminado con éxito"));
        } catch (TrainerNotFoundException e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(Collections.singletonMap("error", "No se encontró el entrenador con DNI " + dni));
        }
    }

}
