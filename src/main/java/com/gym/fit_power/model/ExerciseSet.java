package com.gym.fit_power.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Entity
@Table(name = "exercise_sets")
public class ExerciseSet {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Long id;

    @NotNull
    @Min(1)
    private int reps;

    @NotNull
    @Min(1)
    private int sets;

    @NotNull
    @Min(0)
    private int restInMinutes;

    @ManyToOne
    @JoinColumn(name = "routine_id")
    @NotNull
    private Routine routine;

    @ManyToOne
    @JoinColumn(name = "exercise_id")
    @NotNull
    private Exercise exercise;

}