package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.request.TrainingDiaryRequestDto;
import com.gym.fit_power.dto.response.TrainingDiaryResponseDto;
import com.gym.fit_power.exception.EntityNotFoundException;
import com.gym.fit_power.exception.RoutineNotFoundException;
import com.gym.fit_power.mapper.TrainingDiaryMapper;
import com.gym.fit_power.model.Routine;
import com.gym.fit_power.model.TrainingDiary;
import com.gym.fit_power.repository.ClientRepository;
import com.gym.fit_power.repository.RoutineRepository;
import com.gym.fit_power.repository.TrainingDiaryRepository;
import com.gym.fit_power.service.TrainingDiaryService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class TrainingDiaryServiceImpl implements TrainingDiaryService {

    private final TrainingDiaryRepository trainingDiaryRepository;
    private final RoutineRepository routineRepository;
    private final ClientRepository clientRepository;

    @Override
    public TrainingDiaryResponseDto add(TrainingDiaryRequestDto trainingDiaryRequestDto, String clientCuit) {
        var client = clientRepository.findByCuit(clientCuit);
        if (client == null) {
            throw new EntityNotFoundException("Cliente con CUIT: " + clientCuit + " no encontrado");
        }
        Optional<Routine> routine = routineRepository.findByClientAndActiveTrue(client);
        if (routine.isEmpty()) {
            throw new RoutineNotFoundException("Rutina no encontrada para el cliente con CUIT: " + clientCuit);
        }
        TrainingDiary trainingDiary = TrainingDiaryMapper.toEntity(trainingDiaryRequestDto);
        trainingDiary.setRoutine(routine.get()); // Asigna el ID de la rutina al objeto TrainingDiary
        return TrainingDiaryMapper.toDto(trainingDiaryRepository.save(trainingDiary));
    }

    @Override
    public List<TrainingDiaryResponseDto> findByRoutineId(Long routineId) {
        return trainingDiaryRepository.findByRoutineId(routineId)
                .stream()
                .map(TrainingDiaryMapper::toDto)
                .toList();
    }

}
