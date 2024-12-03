package com.gym.fit_power.mapper;

import com.gym.fit_power.dto.request.TrainingDiaryRequestDto;
import com.gym.fit_power.dto.response.TrainingDiaryResponseDto;
import com.gym.fit_power.model.TrainingDiary;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

@Component
public class TrainingDiaryMapper {

    private static final String DATE_FORMAT = "dd-MM-yyyy HH:mm:ss";

    public static TrainingDiary toEntity(TrainingDiaryRequestDto dto) {
        if (dto == null) {
            throw new NullPointerException("dto cannot be null");
        }
        TrainingDiary trainingDiary = new TrainingDiary();
        trainingDiary.setCommentary(dto.getCommentary());
        return trainingDiary;
    }

    public static TrainingDiaryResponseDto toDto(TrainingDiary trainingDiary) {
        return TrainingDiaryResponseDto.builder()
                .id(trainingDiary.getId())
                .commentary(trainingDiary.getCommentary())
                .createdAt(trainingDiary.getCreatedAt().format(DateTimeFormatter.ofPattern(DATE_FORMAT)))
                .routineId(trainingDiary.getRoutine().getId())
                .build();
    }

    private static String formatCreatedAt(LocalDateTime createdAt) {
        return createdAt.format(DateTimeFormatter.ofPattern(DATE_FORMAT));
    }

}
