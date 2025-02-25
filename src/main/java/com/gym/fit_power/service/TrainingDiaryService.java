package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.TrainingDiaryRequestDto;
import com.gym.fit_power.dto.response.TrainingDiaryResponseDto;
import com.gym.fit_power.exception.EntityNotFoundException;

import java.util.List;

public interface TrainingDiaryService {
    TrainingDiaryResponseDto add(TrainingDiaryRequestDto trainingDiaryRequestDto, String clientCuit) throws EntityNotFoundException;
    List<TrainingDiaryResponseDto> findByRoutineId(Long routineId);
}
