package com.gym.fit_power.controller;

import com.gym.fit_power.dto.NutriPlanDTO;
import com.gym.fit_power.service.impl.NutriPlanServiceImpl;
import jakarta.validation.Valid;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import static com.gym.fit_power.constant.NutritinistConstants.CREATE_NUTRIPLAN;
import static com.gym.fit_power.constant.NutritinistConstants.FINDONE;

@RestController
@RequestMapping("/NutririonPlan")
public class NutritionPlanController {
    private final NutriPlanServiceImpl service;
    protected static final Logger logger = LoggerFactory.getLogger(NutritionPlanController.class);

    public NutritionPlanController(NutriPlanServiceImpl service) {
        this.service = service;
    }

    @PostMapping("/savePlan")
    public ResponseEntity<NutriPlanDTO> saveNutriPlan(@Valid @RequestBody NutriPlanDTO request) {
        logger.info("{} para el cliente {}", request.getClientCuit(), CREATE_NUTRIPLAN);
        if (service.readOne(request.getId()) != null) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
        NutriPlanDTO response = service.create(request);
        return new ResponseEntity<>(response, HttpStatus.CREATED);
    }

    @GetMapping("/readOnePlan/{id}")
    public ResponseEntity<NutriPlanDTO> readOneNutriPlan(@PathVariable Long id){
        logger.info(FINDONE+ "{} ",id);
        if (service.readOne(id) != null){
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
        NutriPlanDTO response = service.readOne(id);
        return new ResponseEntity<>(response,HttpStatus.OK);
    }
    @PatchMapping("/disablePlan/{id}")
    public ResponseEntity<NutriPlanDTO> disablePlan(@PathVariable Long id){
        if (service.readOne(id) != null){
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
        NutriPlanDTO response = service.disable(id);
        return new ResponseEntity<>(response,HttpStatus.OK);
    }

    @PatchMapping("/enablePlan/{id}")
    public ResponseEntity<NutriPlanDTO> enablePlan(@PathVariable Long id){
        if (service.readOne(id) != null){
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
        NutriPlanDTO response = service.enable(id);
        return new ResponseEntity<>(response,HttpStatus.OK);
    }


}
