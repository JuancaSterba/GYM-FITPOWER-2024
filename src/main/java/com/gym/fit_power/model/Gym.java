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
    Long id;

    @Column(unique = true)
    String code;

    String domain;
    String address;
    String mail;
    String phone;
    GregorianCalendar createdAt;
    GregorianCalendar updatedAt;
    Boolean active;

}
