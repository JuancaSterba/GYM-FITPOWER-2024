package com.gym.fit_power.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.ColumnDefault;

import java.time.LocalDate;
import java.time.LocalDateTime;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "routines")
public class Routine {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Long id;

    @NotNull
    @NotEmpty
    @Size(min = 3, max = 20)
    @Pattern(regexp = "^[a-zA-Z0-9]+$")
    private String code;

    @NotNull
    @NotEmpty
    @Size(min = 3, max = 50)
    private String name;

    @NotNull
    @NotEmpty
    @Size(min = 10, max = 200)
    private String description;

    @NotNull
    @NotEmpty
    @Size(min = 10, max = 200)
    private String goals;

    @NotNull
    @NotEmpty
    @Pattern(regexp = "^(Fácil|Medio|Difícil)$")
    private String difficulty;

    @Column(name = "created_at")
    private LocalDate createdAt;

    @Column(name = "updated_at")
    private LocalDate updatedAt;

    @NotNull
    private boolean active;

    @ManyToOne(cascade = CascadeType.ALL, optional = false)
    @JoinColumn(name = "trainer_id", nullable = false)
    private Trainer trainer;

    @ManyToOne(cascade = CascadeType.ALL, optional = false)
    @JoinColumn(name = "customer_id", nullable = false)
    private Customer customer;

    @PrePersist
    private void prePersist() {
        createdAt = LocalDate.now();
        active = true;
    }

}