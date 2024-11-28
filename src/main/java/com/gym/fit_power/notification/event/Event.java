package com.gym.fit_power.notification.event;

import lombok.Data;
import lombok.ToString;

import java.time.LocalDateTime;

@Data
@ToString
public abstract class Event<E> {
    private String id;
    private LocalDateTime date;
    private EventType type;
    private E entity;
}
