package com.gym.fit_power.model;

import lombok.Data;


import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import java.util.GregorianCalendar;
import java.util.List;

@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "nutritionists")
public class Nutritionist {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    Long id;

    private String name;
    private String lastname;

    @Column(unique = true)
    private String cuit;

    private String email;
    private String phone;
    private String speciality;
    private GregorianCalendar birthdate;
    private GregorianCalendar createdAt;
    private GregorianCalendar updatedAt;
    private Boolean enabled;



}
