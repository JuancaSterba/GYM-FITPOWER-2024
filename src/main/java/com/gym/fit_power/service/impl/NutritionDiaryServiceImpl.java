package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.NutritionDiaryDTO;

import com.gym.fit_power.exception.EntitySaveException;
import com.gym.fit_power.model.Client;
import com.gym.fit_power.model.NutritionDiary;
import com.gym.fit_power.model.NutritionPlan;
import com.gym.fit_power.repository.ClientRepository;
import com.gym.fit_power.repository.NutritioDiaryRepository;
import com.gym.fit_power.repository.NutritionPlanRepository;
import com.gym.fit_power.service.NutritionDiaryService;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

import static com.gym.fit_power.constant.NutritinistConstants.*;

@Service
@Slf4j
public class NutritionDiaryServiceImpl implements NutritionDiaryService {
    private final NutritioDiaryRepository repository;
    private final NutritionPlanRepository nutritionPlanRepository;
    private final ClientRepository clientRepository;
    protected static final Logger logger = LoggerFactory.getLogger(NutritionDiaryServiceImpl.class);

    public NutritionDiaryServiceImpl(NutritioDiaryRepository repository, NutritionPlanRepository nutritionPlanRepository, ClientRepository clientRepository) {
        this.repository = repository;
        this.nutritionPlanRepository = nutritionPlanRepository;
        this.clientRepository = clientRepository;
    }


    @Override
    public NutritionDiaryDTO update(String cuit, NutritionDiaryDTO request) {
        try {
            //buscar el plan activo del cliente
            Client client = clientRepository.findByCuit(cuit);
            NutritionPlan nutritionPlanActive = client.getPlans().stream()
                    .filter(NutritionPlan::getEnabled)
                    .findFirst()
                    .orElseThrow();
            //busca si existe un plan del dia de la fecha y si no existe lo crea
            NutritionDiary log = nutritionPlanActive.getLogNutri().stream()
                    .filter(nd -> nd.getCreatedAt().toLocalDate().equals(LocalDate.now()))
                    .findFirst()
                    .orElse(new NutritionDiary());
            //setea los datos
            NutritionDiary nutritionDiaryUpdate = toEntity(request);
            log.setCreatedAt(LocalDateTime.now());
            log.setBreakfast(nutritionDiaryUpdate.getBreakfast());
            log.setLunch(nutritionDiaryUpdate.getLunch());
            log.setSnacks(nutritionDiaryUpdate.getSnacks());
            log.setDinner(nutritionDiaryUpdate.getDinner());
            log.setActualWeight(nutritionDiaryUpdate.getActualWeight());
            log.setNutritionPlan(nutritionDiaryUpdate.getNutritionPlan());
            repository.save(log);
            logger.info(SUCESSFULLY_UPDATE);
            return toDto(log);
        } catch (Exception e) {
            throw new EntitySaveException(e.getMessage());
        }
    }

    @Override
    public List<NutritionDiaryDTO> readByNutritionPlan(String cuit, Long id) {
        Client client = clientRepository.findByCuit(cuit);
        NutritionPlan nutritionPlan = client.getPlans().stream()
                .filter(nutritionPlan1 -> nutritionPlan1.getId().equals(id))
                .findFirst()
                .orElseThrow();
        
       return nutritionPlan.getLogNutri().stream()
                .map(this::toDto)
                .toList();
    }
    
    @Override
    public List<NutritionDiaryDTO> readByClientActivePlan(String clientCuit) {
        Client client = clientRepository.findByCuit(clientCuit);
        return client.getPlans().stream()
                .filter(NutritionPlan::getEnabled)
                .findFirst()
                .orElseThrow().getLogNutri().stream()
                .map(this::toDto)
                .toList();
    }

    private NutritionDiary toEntity(NutritionDiaryDTO request) {
        NutritionDiary nutritionDiary = new NutritionDiary();
        nutritionDiary.setCreatedAt(LocalDateTime.now());
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

    private NutritionDiaryDTO toDto(NutritionDiary nutritionDiary) {
        NutritionDiaryDTO nutritionDiaryDTO = new NutritionDiaryDTO();
        nutritionDiaryDTO.setId(nutritionDiary.getId());
        nutritionDiaryDTO.setUpdateAt(nutritionDiary.getCreatedAt().toString());
        nutritionDiaryDTO.setBreakfast(nutritionDiary.getBreakfast());
        nutritionDiaryDTO.setLunch(nutritionDiary.getLunch());
        nutritionDiaryDTO.setSnacks(nutritionDiary.getSnacks());
        nutritionDiaryDTO.setDinner(nutritionDiary.getDinner());
        nutritionDiaryDTO.setActualWeight(nutritionDiary.getActualWeight());
        return nutritionDiaryDTO;
    }

}
