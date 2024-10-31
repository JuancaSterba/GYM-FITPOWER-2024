package com.gym.fit_power.model;

import lombok.Data;
import java.util.List;
import java.time.LocalDate;
import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

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

    @OneToMany(mappedBy = "client")
    private List<Routine> routines;

    @OneToMany(mappedBy = "client")
    private List<NutritionPlan> plans;

    private String name;
    private String lastname;
    private String email;
    private String phone;
    private LocalDate birthDate;
    private Boolean enabled;

}
