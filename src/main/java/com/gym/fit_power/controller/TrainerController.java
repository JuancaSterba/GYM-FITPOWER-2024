package com.gym.fit_power.controller;

import com.gym.fit_power.dto.request.TrainerRequestDto;
import com.gym.fit_power.dto.response.TrainerResponseDto;
import com.gym.fit_power.service.TrainerService;
import com.gym.fit_power.util.ResponseUtils;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("api/v1/trainer")
@Validated
@Slf4j
public class TrainerController {

    private final TrainerService trainerService;

    public TrainerController(TrainerService trainerService, ResponseUtils responseUtils) {
        this.trainerService = trainerService;
    }

    @GetMapping
    public ResponseEntity<List<TrainerResponseDto>> findAll() {
        log.info("Iniciando la búsqueda de todos los entrenadores."); // Log de inicio
        var trainers = trainerService.findAll();
        log.info("Se encontraron {} entrenadores.", trainers.size()); // Log del resultado
        return ResponseEntity.ok(trainers); // Devolver la lista de entrenadores
    }

    @GetMapping("/{cuit}")
    public ResponseEntity<TrainerResponseDto> findByDni(@PathVariable String cuit) {
        log.info("Buscando entrenador con CUIT: {}", cuit); // Log de búsqueda
        TrainerResponseDto trainer = trainerService.findByCuit(cuit);
        log.info("Entrenador encontrado: {}", trainer); // Log del resultado
        return ResponseEntity.ok(trainer); // Devolver el entrenador encontrado
    }

    @PostMapping
    public ResponseEntity<TrainerResponseDto> save(@RequestBody @Valid TrainerRequestDto trainerRequestDto) {
        log.info("Intentando crear entrenador con CUIT: {}", trainerRequestDto.getCuit()); // Log de creación
        TrainerResponseDto createdTrainer = trainerService.save(trainerRequestDto);
        log.info("Entrenador creado con éxito: {}", createdTrainer); // Log de éxito
        return ResponseEntity.status(HttpStatus.CREATED).body(createdTrainer); // Devolver el objeto creado
    }

    @PutMapping("/{cuit}")
    public ResponseEntity<TrainerResponseDto> update(@PathVariable String cuit, @RequestBody @Valid TrainerRequestDto trainerRequestDto) {
        log.info("Actualizando entrenador con CUIT: {}", cuit); // Log de actualización
        TrainerResponseDto updatedTrainer = trainerService.update(cuit, trainerRequestDto);
        log.info("Entrenador actualizado con éxito: {}", updatedTrainer); // Log de éxito
        return ResponseEntity.ok(updatedTrainer); // Devolver el objeto actualizado
    }

    @DeleteMapping("/{cuit}")
    public ResponseEntity<Map<String, String>> delete(@PathVariable String cuit) {
        log.info("Eliminando entrenador con CUIT: {}", cuit); // Log de eliminación
        trainerService.delete(cuit);
        log.info("Entrenador eliminado con éxito."); // Log de éxito
        return ResponseUtils.createSuccessResponse("Entrenador eliminado con éxito."); // Devolver mensaje de éxito
    }

}
