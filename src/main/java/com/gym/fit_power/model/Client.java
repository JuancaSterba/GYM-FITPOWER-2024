package com.gym.fit_power.model;

import lombok.Data;
import java.util.List;
import java.time.LocalDate;
import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import jakarta.validation.constraints.*;

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
    @JoinColumn(nullable = false)
    private Gym assignedGym;

    @OneToMany(mappedBy = "client")
    private List<Routine> routines;

    @OneToMany(mappedBy = "client")
    private List<NutritionPlan> plans;

    @NotNull
    @NotBlank
    private String name;

    @NotNull
    @NotBlank
    private String lastname;

    @NotNull
    @NotBlank
    private String email;

    @NotNull
    @NotBlank
    private String phone;

    @NotNull
    private LocalDate birthDate;

    @NotNull
    private Boolean enabled;

}
