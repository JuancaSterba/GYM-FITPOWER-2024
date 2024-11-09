package com.gym.fit_power.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
@Entity
@Table(name = "exercises")
public class Exercise {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Long id;

    @NotBlank
    @Size(min = 3, max = 50)
    @Column(unique = true)
    private String name;

    @NotBlank
    @Size(min = 10, max = 200)
    private String description;

    @NotBlank
    @Size(min = 3, max = 50)
    private String muscleGroup;

    @NotNull
    private boolean enabled;

    @PrePersist
    private void prePersist() {
        enabled = true;
    }

}
