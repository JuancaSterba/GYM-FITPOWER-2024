package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.NutritionDiaryDTO;

import com.gym.fit_power.exception.EntityNotFoundException;
import com.gym.fit_power.exception.EntityUpdateException;
import com.gym.fit_power.exception.SaveEntityException;
import com.gym.fit_power.model.NutritionDiary;
import com.gym.fit_power.model.NutritionPlan;
import com.gym.fit_power.repository.NutritioDiaryRepository;
import com.gym.fit_power.repository.NutritionPlanRepository;
import com.gym.fit_power.service.NutritionDiaryService;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

import static com.gym.fit_power.constant.NutritinistConstants.*;

@Service
@Slf4j
public class NutritionDiaryServiceImpl implements NutritionDiaryService{
    private final NutritioDiaryRepository repository;
    private final NutritionPlanRepository nutritionPlanRepository;
    protected static final Logger logger = LoggerFactory.getLogger(NutritionDiaryServiceImpl.class);

    public NutritionDiaryServiceImpl(NutritioDiaryRepository repository, NutritionPlanRepository nutritionPlanRepository) {
        this.repository = repository;
        this.nutritionPlanRepository = nutritionPlanRepository;
    }

    @Override
    public NutritionDiaryDTO create(NutritionDiaryDTO request) {
        NutritionDiaryDTO nutritionDiaryDTO;
        try {
            nutritionDiaryDTO = toDto(repository.save(toEntity(request)));
            logger.info("{} {}",CREATE_NUTRIDIARYLOG , request.getId());
        } catch (Exception e) {
            throw new SaveEntityException();
        }
        return nutritionDiaryDTO;
    }

    @Override
    public NutritionDiaryDTO update(Long id, NutritionDiaryDTO request) {
        try {
            NutritionDiary nutritionDiary = repository.findById(id)
                    .orElseThrow(EntityNotFoundException::new);

            NutritionDiary nutritionDiaryUpdate = toEntity(request);
            nutritionDiary.setUpdateAt(LocalDateTime.now());  // Actualiza la fecha de modificación
            nutritionDiary.setBreakfast(nutritionDiaryUpdate.getBreakfast());
            nutritionDiary.setLunch(nutritionDiaryUpdate.getLunch());
            nutritionDiary.setSnacks(nutritionDiaryUpdate.getSnacks());
            nutritionDiary.setDinner(nutritionDiaryUpdate.getDinner());
            nutritionDiary.setActualWeight(nutritionDiaryUpdate.getActualWeight()); // Corrige este valor
            nutritionDiary.setNutritionPlan(nutritionDiaryUpdate.getNutritionPlan());
            repository.save(nutritionDiary);
            logger.info(SUCESSFULLY_UPDATE);
            return toDto(nutritionDiary);
        } catch (Exception e) {
            throw new EntityUpdateException(e.getMessage());
        }
    }

    private NutritionDiary toEntity (NutritionDiaryDTO request){
        NutritionDiary nutritionDiary = new NutritionDiary();
        nutritionDiary.setUpdateAt(LocalDateTime.now());
        nutritionDiary.setBreakfast(request.getBreakfast());
        nutritionDiary.setLunch(request.getLunch());
        nutritionDiary.setSnacks(request.getSnacks());
        nutritionDiary.setDinner(request.getDinner());
        nutritionDiary.setActualWeight(request.getActualWeight());
        NutritionPlan nutritionPlan;
        nutritionPlan = nutritionPlanRepository.findAll().stream()
                .filter(nutritionPlan1 -> nutritionPlan1.getId().equals(request.getId()))
                .findFirst()
                .orElseThrow(null);
        nutritionDiary.setNutritionPlan(nutritionPlan);
        return nutritionDiary;
    }

    private NutritionDiaryDTO toDto (NutritionDiary nutritionDiary){
        NutritionDiaryDTO nutritionDiaryDTO = new NutritionDiaryDTO();
        nutritionDiaryDTO.setId(nutritionDiary.getId());
        nutritionDiaryDTO.setUpdateAt(nutritionDiary.getUpdateAt().toString());
        nutritionDiaryDTO.setBreakfast(nutritionDiary.getBreakfast());
        nutritionDiaryDTO.setLunch(nutritionDiary.getLunch());
        nutritionDiaryDTO.setSnacks(nutritionDiary.getSnacks());
        nutritionDiaryDTO.setDinner(nutritionDiary.getDinner());
        nutritionDiaryDTO.setActualWeight(nutritionDiary.getActualWeight());
        return nutritionDiaryDTO;
    }

    }
