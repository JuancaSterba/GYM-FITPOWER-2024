package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.ClientDTO;
import com.gym.fit_power.dto.request.ExerciseSetRequestDto;
import com.gym.fit_power.dto.request.RoutineRequestDto;
import com.gym.fit_power.dto.request.TrainerRequestDto;
import com.gym.fit_power.dto.response.RoutineResponseDto;
import com.gym.fit_power.dto.response.TrainerResponseDto;
import com.gym.fit_power.exception.EntityNotFoundException;
import com.gym.fit_power.exception.RoutineNotFoundException;
import com.gym.fit_power.exception.TrainerNotFoundException;
import com.gym.fit_power.mapper.ExerciseSetMapper;
import com.gym.fit_power.mapper.RoutineMapper;
import com.gym.fit_power.mapper.TrainerMapper;
import com.gym.fit_power.model.*;
import com.gym.fit_power.repository.*;
import com.gym.fit_power.service.ClientService;
import com.gym.fit_power.service.RoutineService;
import com.gym.fit_power.service.TrainerService;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class RoutineServiceImpl implements RoutineService {

    private final RoutineRepository routineRepository;
    private final ClientRepository clientRepository;
    private final TrainerRepository trainerRepository;
    private final ExerciseRepository exerciseRepository;
    private final RoutineMapper routineMapper;

    public RoutineServiceImpl(RoutineRepository routineRepository, ClientRepository clientRepository, TrainerRepository trainerRepository, ExerciseRepository exerciseRepository, RoutineMapper routineMapper) {
        this.routineRepository = routineRepository;
        this.clientRepository = clientRepository;
        this.trainerRepository = trainerRepository;
        this.exerciseRepository = exerciseRepository;
        this.routineMapper = routineMapper;
    }

    @Override
    public RoutineResponseDto save(RoutineRequestDto routineRequestDto, String trainerCuit, String clientCuit) {

        Optional<Trainer> trainer = trainerRepository.findByCuit(trainerCuit);
        if (trainer.isEmpty()) {
            throw new TrainerNotFoundException("Trainer with CUIT " + trainerCuit + " not found.");
        }
        Optional<Client> client = clientRepository.findAll()
                .stream()
                .filter(c -> c.getCuit().equals(clientCuit))
                .findFirst();
        if (client.isEmpty()) {
            throw new TrainerNotFoundException("Client with CUIT " + clientCuit + " not found.");
        }
        Routine routine = routineMapper.toEntity(routineRequestDto);
        routine.setTrainer(trainer.get());
        routine.setClient(client.get());

        List<ExerciseSetRequestDto> exerciseSetRequestDtos = routineRequestDto.getExerciseSets();
        List<ExerciseSet> exerciseSets = new ArrayList<>();
        for (ExerciseSetRequestDto exerciseSetRequestDto : exerciseSetRequestDtos) {
            Exercise exercise = exerciseRepository.findById(exerciseSetRequestDto.getExerciseId())
                    .orElseThrow(() -> new RuntimeException("Exercise not found"));
            ExerciseSet exerciseSet = ExerciseSetMapper.toEntity(exerciseSetRequestDto, routine, exercise);
            exerciseSets.add(exerciseSet);
        }
        routine.setExerciseSets(exerciseSets);

        RoutineResponseDto activeRoutineDto = findClientActiveRoutine(clientCuit);
        if (activeRoutineDto != null) {
            disableActiveRoutine(activeRoutineDto.getId());
        }

        Routine savedRoutine = routineRepository.save(routine);
        return RoutineMapper.toDto(savedRoutine);
    }

    @Override
    public RoutineResponseDto findClientActiveRoutine(String clientCuit) {
        Optional<Client> client = clientRepository.findAll()
                .stream()
                .filter(c -> c.getCuit().equals(clientCuit))
                .findFirst();
        if (client.isEmpty()) {
            throw new EntityNotFoundException("Client with CUIT " + clientCuit + " not found.");
        }
        Optional<Routine> routine = routineRepository.findByClientAndActiveTrue(client.get());
        if (routine.isEmpty()) {
            throw new RoutineNotFoundException("Routine not found.");
        }
        return RoutineMapper.toDto(routine.get());
    }

    @Override
    public void disableActiveRoutine(Long routineId) {
        routineRepository.findById(routineId)
                .ifPresent(r -> {
                    r.setActive(false);
                    routineRepository.save(r);
                });
    }

}
