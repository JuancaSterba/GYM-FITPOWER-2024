package com.gym.fit_power.service;

import com.gym.fit_power.dto.request.TrainingDiaryRequestDto;
import com.gym.fit_power.dto.response.TrainingDiaryResponseDto;
import com.gym.fit_power.exception.RoutineNotFoundException;

public interface TrainingDiaryService {
    TrainingDiaryResponseDto save(TrainingDiaryRequestDto trainingDiaryRequestDto) throws RoutineNotFoundException;
}
