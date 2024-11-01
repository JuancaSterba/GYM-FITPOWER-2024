package com.gym.fit_power.service.impl;

import com.gym.fit_power.dto.NutriPlanDTO;
import com.gym.fit_power.exception.EntityNotFoundException;
import com.gym.fit_power.exception.SaveEntityException;
import com.gym.fit_power.model.Client;
import com.gym.fit_power.model.NutritionPlan;
import com.gym.fit_power.model.Nutritionist;
import com.gym.fit_power.repository.ClientRepository;
import com.gym.fit_power.repository.NutriRepository;
import com.gym.fit_power.repository.NutritionPlanRepository;
import com.gym.fit_power.service.NutriPlanService;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Optional;

import static com.gym.fit_power.constant.NutritinistConstants.SUCCESSFUL;

@Service
@Slf4j
public class NutriPlanServiceImpl implements NutriPlanService {

    private final NutritionPlanRepository repository;
    private final NutriRepository nutriRepository;
    private final ClientRepository clientRepository;
    protected static final Logger logger = LoggerFactory.getLogger(NutriPlanServiceImpl.class);


    public NutriPlanServiceImpl(NutritionPlanRepository repository, NutriRepository nutriRepository, ClientRepository clientRepository) {
        this.repository = repository;
        this.nutriRepository = nutriRepository;
        this.clientRepository = clientRepository;
    }

    @Override
    @Transactional
    public NutriPlanDTO create(NutriPlanDTO request) {
        try {
            NutritionPlan nutritionPlan = toEntity(request);
            nutritionPlan.setLogNutri(new ArrayList<>());
            NutriPlanDTO response = toDTO(repository.save(nutritionPlan));
            logger.info(SUCCESSFUL);
            return response;
        } catch (Exception e) {
            throw new SaveEntityException(e.getMessage());
        }
    }

    @Override
    @Transactional
    public NutriPlanDTO readOne(Long id) {
        Optional<NutritionPlan> nutritionPlanOptional = repository.findById(id);
        NutritionPlan nutritionPlan = nutritionPlanOptional.orElseThrow(EntityNotFoundException::new);
        return toDTO(nutritionPlan);
    }


    @Override
    @Transactional
    public NutriPlanDTO disable(Long id) {
        Optional<NutritionPlan> nutritionPlanOptional = repository.findById(id);
        NutritionPlan nutritionPlan = nutritionPlanOptional.orElseThrow(EntityNotFoundException::new);
        nutritionPlan.setEnabled(false);
        repository.save(nutritionPlan);
        return toDTO(nutritionPlan);
    }

    @Override
    @Transactional
    public NutriPlanDTO enable(Long id) {
        Optional<NutritionPlan> nutritionPlanOptional = repository.findById(id);
        NutritionPlan nutritionPlan = nutritionPlanOptional.orElseThrow(EntityNotFoundException::new);
        nutritionPlan.setEnabled(true);
        repository.save(nutritionPlan);
        return toDTO(nutritionPlan);
    }

    private NutritionPlan toEntity(NutriPlanDTO request) {
        NutritionPlan nutriP = new NutritionPlan();
        nutriP.setCreatAt(LocalDate.now());
        nutriP.setDailyCalories(request.getDailyCalories());
        nutriP.setDailyCarbohydrates(request.getDailyCarbohydrates());
        nutriP.setDailyFats(request.getDailyFats());
        nutriP.setDailyProteins(request.getDailyProteins());
        nutriP.setDesiredWeight(request.getDesiredWeight());
        Nutritionist nutri;
        String cuit = request.getNutritionistCuit();
        nutri = nutriRepository.findAll().stream()
                .filter(nutritionist -> nutritionist.getCuit().equals(cuit))
                .findFirst()
                .orElse(null);
        nutriP.setNutritionist(nutri);
        Client client;
        String clientCuit = request.getClientCuit();
        client = clientRepository.findAll().stream()
                .filter(client1 -> client1.getCuit().equals(clientCuit))
                .findFirst()
                .orElse(null);
        nutriP.setClient(client);
        return nutriP;
    }

    private NutriPlanDTO toDTO(NutritionPlan nutritionPlan) {
        NutriPlanDTO response = new NutriPlanDTO();
        response.setDailyProteins(nutritionPlan.getDailyProteins());
        response.setDailyCalories(nutritionPlan.getDailyCalories());
        response.setDailyCarbohydrates(nutritionPlan.getDailyCarbohydrates());
        response.setDesiredWeight(nutritionPlan.getDesiredWeight());
        response.setDailyFats(nutritionPlan.getDailyFats());
        response.setClientCuit(nutritionPlan.getClient().getName());
        response.setNutritionistCuit(nutritionPlan.getNutritionist().getName());
        return response;

    }
}
