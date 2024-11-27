package com.gym.fit_power.model;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name="gyms")
public class Gym {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(unique = true)
    private String address;

    @NotNull
    @NotBlank
    private String domain;

    @NotNull
    @NotBlank
    private String mail;

    @NotNull
    @NotBlank
    private String phone;

    private Boolean enabled;

}
