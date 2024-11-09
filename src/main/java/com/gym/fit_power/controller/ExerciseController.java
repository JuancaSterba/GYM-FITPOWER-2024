package com.gym.fit_power.controller;

import com.gym.fit_power.dto.request.ExerciseRequestDto;
import com.gym.fit_power.dto.response.ExerciseResponseDto;
import com.gym.fit_power.service.ExerciseService;
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
@RequestMapping("api/v1/exercise")
@Validated
@Slf4j
public class ExerciseController {

    private final ExerciseService exerciseService;

    public ExerciseController(ExerciseService exerciseService, ResponseUtils responseUtils) {
        this.exerciseService = exerciseService;
    }

    @GetMapping
    public ResponseEntity<List<ExerciseResponseDto>> findAll() {
        log.info("Iniciando la búsqueda de todos los ejercicios."); // Log de inicio
        var exercises = exerciseService.findAll();
        log.info("Se encontraron {} ejercicios.", exercises.size()); // Log del resultado
        return ResponseEntity.ok(exercises); // Devolver la lista de ejercicios
    }

    @GetMapping("/{name}")
    public ResponseEntity<ExerciseResponseDto> findByName(@PathVariable String name) {
        log.info("Buscando ejercicio con nombre: {}", name); // Log de búsqueda
        ExerciseResponseDto exercise = exerciseService.findByName(name);
        log.info("Ejercicio encontrado: {}", exercise); // Log del resultado
        return ResponseEntity.ok(exercise); // Devolver el ejercicio encontrado
    }

    @PostMapping
    public ResponseEntity<ExerciseResponseDto> save(@RequestBody @Valid ExerciseRequestDto exerciseRequestDto) {
        log.info("Intentando crear ejercicio con nombre: {}", exerciseRequestDto.getName()); // Log de creación
        ExerciseResponseDto createdExercise = exerciseService.save(exerciseRequestDto);
        log.info("Ejercicio creado con éxito: {}", createdExercise); // Log de suceceso
        return ResponseEntity.status(HttpStatus.CREATED).body(createdExercise); // Devolver el objeto creado
    }

    @PutMapping("/{name}")
    public ResponseEntity<ExerciseResponseDto> update(@PathVariable String name, @RequestBody @Valid ExerciseRequestDto exerciseRequestDto) {
        log.info("Actualizando ejercicio con nombre: {}", name); // Log de actualización
        ExerciseResponseDto updatedExercise = exerciseService.update(name, exerciseRequestDto);
        log.info("Ejercicio actualizado con éxito: {}", updatedExercise); // Log de suceceso
        return ResponseEntity.ok(updatedExercise); // Devolver el objeto actualizado
    }

    @DeleteMapping("/{name}")
    public ResponseEntity<Map<String, String>> delete(@PathVariable String name) {
        log.info("Intentando eliminar ejercicio con nombre: {}", name); // Log de eliminación
        exerciseService.delete(name);
        log.info("Ejercicio eliminado con éxito."); // Log de suceceso
        return ResponseUtils.createSuccessResponse("Ejercicio eliminado con éxito."); // Devolver una respuesta sin contenido
    }
}
