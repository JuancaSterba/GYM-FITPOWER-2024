package com.gym.fit_power.controller;

import com.gym.fit_power.dto.request.RequestNutri;
import com.gym.fit_power.dto.response.ResponseNutri;
import com.gym.fit_power.service.NutriService;
import jakarta.validation.Valid;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

import static com.gym.fit_power.constant.NutritinistConstants.*;

@RestController
@RequestMapping("/Nutritionist")
public class NutritionistController {

    private final NutriService service;
    protected static final Logger logger = LoggerFactory.getLogger(NutritionistController.class);

    public NutritionistController(NutriService service) {
        this.service = service;
    }

    @PostMapping("/save")
    public ResponseEntity<ResponseNutri> saveNutritionist (@Valid @RequestBody RequestNutri requestNutri){
        logger.info(CREATE +"{}", requestNutri.getId());
        if (service.readOne(requestNutri.getId()) != null){
           return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
        ResponseNutri response = service.create(requestNutri);
        return new ResponseEntity<>(response,HttpStatus.CREATED);
    }


    @GetMapping("/readOne/{id}")
    public ResponseEntity<ResponseNutri> readOneNutritionist(@PathVariable Long id){
        logger.info(FINDONE +"{}", id);
        if (service.readOne(id) != null){
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
        ResponseNutri response = service.readOne(id);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @GetMapping("/readAll")
    public ResponseEntity<List<ResponseNutri>> readAllNutritions(){
        logger.info(FINDALL);
        List<ResponseNutri>nutris = service.readAll();
        return new ResponseEntity<>(nutris,HttpStatus.OK);
    }

    @PutMapping("/update/{id}")
    public ResponseEntity<ResponseNutri> updateNutritionist(@PathVariable Long id, @RequestBody RequestNutri requestNutri){
        logger.info(UPDATE +"{} {}", id ,requestNutri.getName());
        if (service.readOne(id) != null){
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
        ResponseNutri response = service.update(id,requestNutri);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @DeleteMapping("/disable/{id}")
    public ResponseEntity<ResponseNutri> desableNutritionist(@PathVariable Long id){
        logger.info(DISABLE+ "{}", id);
        if (service.readOne(id) != null){
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
        ResponseNutri response = service.disable(id);
        return new ResponseEntity<>(response,HttpStatus.OK);

    }

    @PatchMapping("/enabled/{id}")
    public ResponseEntity<ResponseNutri> enabledNutritionist (@PathVariable Long id){
        logger.info(ENABLED+ "{}", id);
        if (service.readOne(id) != null){
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }
        ResponseNutri response = service.enable(id);
        return new ResponseEntity<>(response,HttpStatus.OK);
    }



}
