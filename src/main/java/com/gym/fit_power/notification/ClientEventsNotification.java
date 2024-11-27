package com.gym.fit_power.notification;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import com.gym.fit_power.notification.event.Event;
import org.springframework.kafka.annotation.KafkaListener;
import com.gym.fit_power.notification.event.ClientCreatedEvent;

@Slf4j
@Component
public class ClientEventsNotification{

    private final MailSenderService mailSender;

    public ClientEventsNotification(MailSenderService mailSender){
        this.mailSender = mailSender;
    }

    @KafkaListener(topics = "${topic.client.name:clients}",
            containerFactory = "kafkaListenerContainerFactory", groupId = "group1")
    public void clientEventConsumer(Event<?> event) {
        if (event.getClass().isAssignableFrom(ClientCreatedEvent.class)) {
            ClientCreatedEvent createdEvent = (ClientCreatedEvent) event;
            log.info("Received created event for client with Id={}, data={}",
                    createdEvent.getId(), createdEvent.getData().toString());
            mailSender.send(createdEvent.getData().getEmail(),
                    "Bienvenido a FitPower!!",
                    "Se ha recibido la solicitud de creación para un nuevo cliente en la aplicación FitPower\n" +
                            "si desconoce esta solicitud por favor comuníquese con el servicio técnico.");
        }
    }
}
