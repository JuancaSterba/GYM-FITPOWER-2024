package com.gym.fit_power.notification.event;

import com.gym.fit_power.model.Client;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = true)
public class ClientUpdatedEvent extends Event<Client> {
}