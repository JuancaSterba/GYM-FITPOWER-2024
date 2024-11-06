package com.gym.fit_power.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDate;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "trainers")
public class Trainer {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Long id;

    @Column(unique = true)
    @NotNull
    @NotBlank
    private String cuit;

    @NotNull
    @NotBlank
    private String name;

    @NotNull
    @NotBlank
    private String lastname;

    @Column(unique = true)
    @Email
    private String email;

    @Column(name = "phone_number")
    @Pattern(regexp = "^\\d{10}$")
    private String phoneNumber;

    @Column(name = "created_at")
    private LocalDate createdAt;

    @NotNull
    private boolean enabled;

    @PrePersist
    private void prePersist() {
        createdAt = LocalDate.now();
        enabled = true;
    }

}
