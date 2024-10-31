package com.gym.fit_power.model;

import lombok.Data;
import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import java.util.GregorianCalendar;

@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name="gyms")
public class Gym {

    @Id
    @GeneratedValue(strategy= GenerationType.IDENTITY)
    private Long id;

    @Column(unique = true)
    private String code;

    private String domain;
    private String address;
    private String mail;
    private String phone;
    private GregorianCalendar createdAt;
    private GregorianCalendar updatedAt;
    private Boolean enabled;

}
