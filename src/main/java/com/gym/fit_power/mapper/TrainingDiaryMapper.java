package com.gym.fit_power.mapper;

import com.gym.fit_power.dto.request.TrainingDiaryRequestDto;
import com.gym.fit_power.dto.response.TrainingDiaryResponseDto;
import com.gym.fit_power.model.TrainingDiary;
import org.springframework.stereotype.Component;

@Component
public class TrainingDiaryMapper {
    public static TrainingDiary toEntity(TrainingDiaryRequestDto dto) {
        TrainingDiary trainingDiary = new TrainingDiary();
        trainingDiary.setCommentary(dto.getCommentary());
        return trainingDiary;
    }

    public static TrainingDiaryResponseDto toDto(TrainingDiary trainingDiary) {
        return TrainingDiaryResponseDto.builder()
                .id(trainingDiary.getId())
                .comentary(trainingDiary.getCommentary())
                .createdAt(trainingDiary.getCreatedAt().toString())
                .routineId(trainingDiary.getRoutine().getId())
                .build();
    }
}
