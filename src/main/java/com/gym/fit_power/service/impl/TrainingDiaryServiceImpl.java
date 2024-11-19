package com.gym.fit_power.service.impl;

import com.gym.fit_power.repository.TrainingDiaryRepository;
import com.gym.fit_power.service.TrainingDiaryService;
import org.springframework.stereotype.Service;

@Service
public class TrainingDiaryServiceImpl implements TrainingDiaryService {

    private final TrainingDiaryRepository trainingDiaryRepository;

    public TrainingDiaryServiceImpl(TrainingDiaryRepository trainingDiaryRepository) {
        this.trainingDiaryRepository = trainingDiaryRepository;
    }

    
}
