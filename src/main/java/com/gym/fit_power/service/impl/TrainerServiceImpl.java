package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.request.TrainerRequestDto;
import com.gym.fit_power.dto.response.TrainerResponseDto;
import com.gym.fit_power.exception.DuplicatedTrainerException;
import com.gym.fit_power.exception.TrainerNotFoundException;
import com.gym.fit_power.exception.TrainerUpdateException;
import com.gym.fit_power.model.Trainer;
import com.gym.fit_power.repository.TrainerRepository;
import com.gym.fit_power.service.TrainerService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;

@Service
public class TrainerServiceImpl implements TrainerService {

    private final TrainerRepository trainerRepository;

    public TrainerServiceImpl(TrainerRepository trainerRepository) {
        this.trainerRepository = trainerRepository;
    }

    @Override
    public List<TrainerResponseDto> findAll() {
        return trainerRepository
                .findAll()
                .stream()
                .map(this::toDto)
                .toList();
    }

    @Override
    @Transactional(readOnly = true)
    public TrainerResponseDto findByCuit(String cuit) throws TrainerNotFoundException {
        Optional<Trainer> trainer = trainerRepository.findByCuit(cuit);
        return trainer.map(this::toDto).orElseThrow(() -> new TrainerNotFoundException("Trainer with CUIT " + cuit + " not found."));
    }

    @Override
    @Transactional
    public TrainerResponseDto save(TrainerRequestDto trainerRequestDto) throws DuplicatedTrainerException {
        if (trainerRepository.existsByCuit(trainerRequestDto.getCuit())) {
            throw new DuplicatedTrainerException("Trainer with CUIT " + trainerRequestDto.getCuit() + " already exists.");
        }
        Trainer trainer = toEntity(trainerRequestDto);
        trainer = trainerRepository.save(trainer);
        return toDto(trainer);
    }

    @Override
    @Transactional
    public TrainerResponseDto update(String cuit, TrainerRequestDto trainerRequestDto) throws TrainerUpdateException {
        if (!trainerRepository.existsByCuit(cuit)) {
            throw new TrainerUpdateException("No trainer found with CUIT " + cuit + " for update.");
        }
        Trainer trainer = toEntity(trainerRequestDto);
        trainer.setCuit(cuit);
        trainer = trainerRepository.save(trainer);
        return toDto(trainer);
    }

    @Override
    @Transactional
    public void delete(String cuit) throws TrainerNotFoundException {
        Optional<Trainer> trainer = trainerRepository.findByCuit(cuit);
        if (trainer.isEmpty()) {
            throw new TrainerNotFoundException("No trainer found with DNI " + cuit + " for deletion.");
        }
        trainer.get().setEnabled(false);
        trainerRepository.save(trainer.get());
    }

    private Trainer toEntity(TrainerRequestDto dto) {
        Trainer trainer = new Trainer();
        trainer.setCuit(dto.getCuit());
        trainer.setName(dto.getName());
        trainer.setLastname(dto.getLastname());
        trainer.setEmail(dto.getEmail());
        trainer.setPhoneNumber(dto.getPhoneNumber());
        return trainer;
    }

    private TrainerResponseDto toDto(Trainer trainer) {
        TrainerResponseDto trainerResponseDto = new TrainerResponseDto();
        trainerResponseDto.setId(trainer.getId());
        trainerResponseDto.setCuit(trainer.getCuit());
        trainerResponseDto.setName(trainer.getName());
        trainerResponseDto.setLastname(trainer.getLastname());
        trainerResponseDto.setEmail(trainer.getEmail());
        trainerResponseDto.setPhoneNumber(trainer.getPhoneNumber());
        trainerResponseDto.setCreatedAt(trainer.getCreatedAt().toString());
        trainerResponseDto.setEnabled(trainer.isEnabled());
        return trainerResponseDto;
    }

}
