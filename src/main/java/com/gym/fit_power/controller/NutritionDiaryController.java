package com.gym.fit_power.controller;

import com.gym.fit_power.dto.NutritionDiaryDTO;
import com.gym.fit_power.service.impl.NutritionDiaryServiceImpl;
import jakarta.validation.Valid;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/NutritionDiary")
public class NutritionDiaryController {
    private final NutritionDiaryServiceImpl service;
    protected static final Logger logger = LoggerFactory.getLogger(NutritionDiaryController.class);

    public NutritionDiaryController(NutritionDiaryServiceImpl service) {
        this.service = service;
    }

    @PostMapping("/saveLog")
    public ResponseEntity<NutritionDiaryDTO> createNutritionDiary(@Valid @RequestBody NutritionDiaryDTO request){
        NutritionDiaryDTO nutritionDiaryDTO = service.create(request);
        return  new ResponseEntity<>(nutritionDiaryDTO, HttpStatus.CREATED);
    }

    @PutMapping("/updateLog")
    public ResponseEntity<NutritionDiaryDTO> updateNutritionDiary (@PathVariable Long id, @RequestBody NutritionDiaryDTO request){
        NutritionDiaryDTO nutritionDiaryDTO = service.update(id,request);
        return new ResponseEntity<>(nutritionDiaryDTO , HttpStatus.OK);
    }
}
