package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.request.ExerciseSetRequestDto;
import com.gym.fit_power.dto.request.RoutineRequestDto;
import com.gym.fit_power.dto.response.RoutineResponseDto;
import com.gym.fit_power.exception.EntityNotFoundException;
import com.gym.fit_power.exception.RoutineNotFoundException;
import com.gym.fit_power.exception.TrainerNotFoundException;
import com.gym.fit_power.mapper.TrainingDiaryMapper;
import com.gym.fit_power.model.*;
import com.gym.fit_power.repository.*;
import com.gym.fit_power.service.RoutineService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RoutineServiceImpl implements RoutineService {

    private final RoutineRepository routineRepository;
    private final ClientRepository clientRepository;
    private final TrainerRepository trainerRepository;
    private final ExerciseRepository exerciseRepository;
    private final ExerciseSetServiceImpl exerciseSetServiceImpl;

    @Override
    public RoutineResponseDto save(RoutineRequestDto routineRequestDto, String clientCuit) {
        var trainerCuit = routineRequestDto.getTrainerCuit();
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
        Routine routine = this.toEntity(routineRequestDto);
        routine.setTrainer(trainer.get());
        routine.setClient(client.get());

        List<ExerciseSetRequestDto> exerciseSetRequestDtos = routineRequestDto.getExerciseSets();
        List<ExerciseSet> exerciseSets = new ArrayList<>();
        for (ExerciseSetRequestDto exerciseSetRequestDto : exerciseSetRequestDtos) {
            Exercise exercise = exerciseRepository.findById(exerciseSetRequestDto.getExerciseId())
                    .orElseThrow(() -> new RuntimeException("Exercise not found"));
            ExerciseSet exerciseSet = exerciseSetServiceImpl.toEntity(exerciseSetRequestDto, routine, exercise);
            exerciseSets.add(exerciseSet);
        }
        routine.setExerciseSets(exerciseSets);

        RoutineResponseDto activeRoutineDto = findClientActiveRoutine(clientCuit);
        if (activeRoutineDto != null) {
            disableActiveRoutine(activeRoutineDto.getId());
        }

        Routine savedRoutine = routineRepository.save(routine);
        return this.toDto(savedRoutine);
    }

    @Override
    public List<RoutineResponseDto> findByClient(String clientCuit) {
        Optional<Client> client = clientRepository.findAll()
                .stream()
                .filter(c -> c.getCuit().equals(clientCuit))
                .findFirst();
        if (client.isEmpty()) {
            throw new EntityNotFoundException("Client with CUIT " + clientCuit + " not found.");
        }
        List<Routine> routines = routineRepository.findByClient(client.get());
        return routines.stream()
                .map(this::toDto)
                .toList();
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
        return this.toDto(routine.get());
    }

    @Override
    public void disableActiveRoutine(Long routineId) {
        routineRepository.findById(routineId)
                .ifPresent(r -> {
                    r.setActive(false);
                    routineRepository.save(r);
                });
    }

    private RoutineResponseDto toDto(Routine routine) {
        return RoutineResponseDto.builder()
                .id(routine.getId())
                .goals(routine.getGoals())
                .createdAt(routine.getCreatedAt().toString())

                .trainerCuit(routine.getTrainer().getCuit())
                .clientCuit(routine.getClient().getCuit())
                .exerciseSets(routine.getExerciseSets()
                        .stream()
                        .map(exerciseSetServiceImpl::toDto)
                        .toList()
                )
                .trainingDiaries((routine.getTrainingDiaries() != null) ? routine.getTrainingDiaries()
                        .stream()
                        .map(TrainingDiaryMapper::toDto)
                        .collect(Collectors.toList()) : Collections.emptyList())
                .build();
    }

    private Routine toEntity(RoutineRequestDto dto) {
        return Routine.builder()
                .goals(dto.getGoals())
                .build();
    }

}
