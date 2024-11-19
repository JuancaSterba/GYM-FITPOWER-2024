package com.gym.fit_power.mapper;

import com.gym.fit_power.dto.request.TrainerRequestDto;
import com.gym.fit_power.dto.response.TrainerResponseDto;
import com.gym.fit_power.model.Trainer;
import org.springframework.stereotype.Component;

@Component
public class TrainerMapper {
    public static Trainer toEntity(TrainerRequestDto dto) {
        Trainer trainer = new Trainer();
        trainer.setCuit(dto.getCuit());
        trainer.setName(dto.getName());
        trainer.setLastname(dto.getLastname());
        trainer.setEmail(dto.getEmail());
        trainer.setPhoneNumber(dto.getPhoneNumber());
        return trainer;
    }

    public static TrainerResponseDto toDto(Trainer trainer) {
        return TrainerResponseDto.builder()
                .id(trainer.getId())
                .cuit(trainer.getCuit())
                .name(trainer.getName())
                .lastname(trainer.getLastname())
                .email(trainer.getEmail())
                .phoneNumber(trainer.getPhoneNumber())
                .createdAt(trainer.getCreatedAt().toString())
                .enabled(trainer.isEnabled())
                .build();
    }
}
