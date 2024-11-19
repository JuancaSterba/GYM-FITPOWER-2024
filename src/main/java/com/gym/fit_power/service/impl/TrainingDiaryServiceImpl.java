package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.request.TrainingDiaryRequestDto;
import com.gym.fit_power.dto.response.TrainingDiaryResponseDto;
import com.gym.fit_power.exception.RoutineNotFoundException;
import com.gym.fit_power.mapper.TrainingDiaryMapper;
import com.gym.fit_power.model.Routine;
import com.gym.fit_power.model.TrainingDiary;
import com.gym.fit_power.repository.RoutineRepository;
import com.gym.fit_power.repository.TrainingDiaryRepository;
import com.gym.fit_power.service.TrainingDiaryService;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class TrainingDiaryServiceImpl implements TrainingDiaryService {

    private final TrainingDiaryRepository trainingDiaryRepository;
    private final RoutineRepository routineRepository;

    public TrainingDiaryServiceImpl(TrainingDiaryRepository trainingDiaryRepository, RoutineRepository routineRepository) {
        this.trainingDiaryRepository = trainingDiaryRepository;
        this.routineRepository = routineRepository;
    }


    @Override
    public TrainingDiaryResponseDto save(TrainingDiaryRequestDto trainingDiaryRequestDto) {
        Optional<Routine> routine = routineRepository.findById(trainingDiaryRequestDto.getRoutineId());
        if (routine.isEmpty()) {
            throw new RoutineNotFoundException("Routine not found");
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
