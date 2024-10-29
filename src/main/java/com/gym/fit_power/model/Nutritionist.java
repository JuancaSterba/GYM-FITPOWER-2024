package com.gym.fit_power.model;

import lombok.Data;

import java.util.List;
import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import java.util.GregorianCalendar;

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

    @OneToMany(mappedBy = "nutritionist")
    private List<Client> clients;

    private String email;
    private String phone;
    private String speciality;
    private GregorianCalendar birthdate;
    private GregorianCalendar createdAt;
    private GregorianCalendar updatedAt;
    private Boolean enabled;

}
