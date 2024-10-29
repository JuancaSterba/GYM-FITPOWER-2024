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
@Table(name = "clients")
public class Client {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(unique = true)
    private String cuit;

    @ManyToOne
    private Gym assignedGym;

    private String name;
    private String lastname;
    private String email;
    private String phone;
    private String genre;
    private String physicalComposition;
    private Float weight;
    private Float height;
    private String corpulence;
    private List<String> allergies;
    private GregorianCalendar birthDate;
    private GregorianCalendar createdAt;
    private GregorianCalendar updatedAt;
    private Boolean enabled;


}
