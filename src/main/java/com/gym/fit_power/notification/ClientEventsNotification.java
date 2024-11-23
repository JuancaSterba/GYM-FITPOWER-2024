package com.gym.fit_power.notification;

import com.gym.fit_power.notification.event.ClientCreatedEvent;
import com.gym.fit_power.notification.event.Event;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.kafka.annotation.KafkaListener;

@Slf4j
@Component
public class ClientEventsNotification{

    @KafkaListener(
            topics = "${topic.client.name:clients}",
            containerFactory = "kafkaListenerContainerFactory",
            groupId = "group1")
    public void consumer(Event<?> event) {
        if (event.getClass().isAssignableFrom(ClientCreatedEvent.class)) {
            ClientCreatedEvent createdEvent = (ClientCreatedEvent) event;
            log.info("Received Customer created event .... with Id={}, data={}",
                    createdEvent.getId(),
                    createdEvent.getData().toString());
        }
    }
}
