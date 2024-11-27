package com.gym.fit_power.notification.event;

import lombok.Data;
import lombok.EqualsAndHashCode;
import com.gym.fit_power.model.Client;

@Data
@EqualsAndHashCode(callSuper = true)
public class ClientCreatedEvent extends Event<Client> {
}
