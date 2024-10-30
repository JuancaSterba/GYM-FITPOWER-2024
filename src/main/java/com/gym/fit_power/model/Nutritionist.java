package com.gym.fit_power.model;


import com.gym.fit_power.util.MyGregorianCalendarConverter;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.GregorianCalendar;
import java.util.List;

@Entity
@Data
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "nutritionists")
public class Nutritionist {
    @Id
    @GeneratedValue(strategy= GenerationType.IDENTITY)
    Long id;

    private String name;
    private String lastname;

    @Column (unique = true)
    private String cuit;

    private String email;
    private String phone;
    private String speciality;

    @Convert(converter = MyGregorianCalendarConverter.class)
    private GregorianCalendar birthdate;

    private GregorianCalendar createdAt;
    private GregorianCalendar updatedAt;

    private Boolean enabled;
    private List<String>clients;


}
