package com.gym.fit_power.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.*;
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

    @NotBlank
    @Column(unique = true)
    private String cuit;

    @NotBlank
    private String name;

    @NotBlank
    private String lastname;

    @Email
    @NotBlank
    @Size(min = 3, max = 50)
    @Column(unique = true)
    private String email;

    @Column(name = "phone_number")
    @NotBlank
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
