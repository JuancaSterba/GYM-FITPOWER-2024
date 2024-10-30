package com.gym.fit_power.model;

import lombok.Data;

import com.gym.fit_power.util.MyGregorianCalendarConverter;
import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import java.util.GregorianCalendar;
import java.util.List;

@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "nutritionists")
public class Nutritionist {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    Long id;

    private String name;
    private String lastname;

    @Column(unique = true)
    private String cuit;

    @OneToMany(mappedBy = "nutritionist")
    private List<Client> clients;

    private String email;
    private String phone;
    private String speciality;

    @Convert(converter = MyGregorianCalendarConverter.class)
    private GregorianCalendar birthdate;

    private GregorianCalendar createdAt;
    private GregorianCalendar updatedAt;

    private Boolean enabled;
    private List<String>clients;


}
