INSERT INTO usuario (username, password, name, lastname, cuit, role) VALUES ('admin', '$2a$10$nkjXuwh1Z5JpdvATkoFmLemrhM1h9FW1hJI//aWVmoTTOCJm4LB8C', 'admin', 'admin', '33672337629', 'ADMIN');
INSERT INTO usuario (username, password, name, lastname, cuit, role) VALUES ('user', '$2a$10$Onmv6UyGj/0DUEay.wjIX.cyao/IQgsy8r8IqSh05D9C2IAi/yWfK', 'user', 'user', '30889977553', 'USER');

INSERT INTO gyms (address, domain, mail, phone, enabled) VALUES ('Calle 123', 'gimnasio.com', 'gimnasio@example', '1234567890', TRUE);
INSERT INTO gyms (address, domain, mail, phone, enabled) VALUES ('Calle 456', 'gimnasio2.com', 'gimnasio2@example', '9876543210', TRUE);
INSERT INTO gyms (address, domain, mail, phone, enabled) VALUES ('Calle 789', 'gimnasio3.com', 'gimnasio3@example', '5555555555', TRUE);

INSERT INTO trainers (cuit, name, lastname, email, phone_number, created_at, enabled) VALUES
('35723456789', 'Anabel', 'López', 'anabel.lopez@hotmail.com', '1558743210', '2021-08-12', TRUE),
('38123456789', 'Carlos', 'Sánchez', 'carlos.sanchez@gmail.com', '1133221100', '2022-04-05', TRUE),
('42923456789', 'Valentina', 'Fernández', 'valentina.fernandez@yahoo.com.ar', '2614567890', '2023-01-20', TRUE),
('45623456789', 'Diego', 'González', 'diego.gonzalez@live.com.ar', '3415678901', '2022-11-15', TRUE),
('49023456789', 'Laura', 'Pérez', 'laura.perez@outlook.com', '1567890123', '2023-05-08', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES
('Sentadillas', 'Ejercicio para fortalecer los músculos de las piernas', 'Piernas', TRUE),
('Peso muerto', 'Ejercicio para fortalecer los músculos de la espalda y piernas', 'Espalda y piernas', TRUE),
('Press de banca', 'Ejercicio para fortalecer los músculos del pecho', 'Pecho', TRUE),
('Remo', 'Ejercicio para fortalecer los músculos de la espalda', 'Espalda', TRUE),
('Elevaciones laterales', 'Ejercicio para fortalecer los músculos de los hombros', 'Hombros', TRUE),
('Extensiones de piernas', 'Ejercicio para fortalecer los músculos de las piernas', 'Piernas', TRUE),
('Curl de bíceps', 'Ejercicio para fortalecer los músculos de los brazos', 'Brazos', TRUE),
('Tríceps', 'Ejercicio para fortalecer los músculos de los brazos', 'Brazos', TRUE),
('Abdominales', 'Ejercicio para fortalecer los músculos del abdomen', 'Abdomen', TRUE),
('Elevaciones de piernas', 'Ejercicio para fortalecer los músculos de las piernas', 'Piernas', TRUE),
('Peso muerto con mancuernas', 'Ejercicio para fortalecer los músculos de la espalda y piernas', 'Espalda y piernas', TRUE),
('Press de hombros', 'Ejercicio para fortalecer los músculos de los hombros', 'Hombros', TRUE),
('Extensiones de brazos', 'Ejercicio para fortalecer los músculos de los brazos', 'Brazos', TRUE),
('Curl de tríceps', 'Ejercicio para fortalecer los músculos de los brazos', 'Brazos', TRUE),
('Elevaciones de hombros', 'Ejercicio para fortalecer los músculos de los hombros', 'Hombros', TRUE),
('Abdominales con peso', 'Ejercicio para fortalecer los músculos del abdomen', 'Abdomen', TRUE),
('Sentadillas con mancuernas', 'Ejercicio para fortalecer los músculos de las piernas', 'Piernas', TRUE),
('Peso muerto con barra', 'Ejercicio para fortalecer los músculos de la espalda y piernas', 'Espalda y piernas', TRUE);

INSERT INTO clients (cuit, assigned_gym_id, name, lastname, email, phone, birth_date, enabled) VALUES
('35723456789', 1, 'Lucas', 'Moreno', 'lucas.moreno@gmail.com', '1558743210', '1992-11-05', TRUE),
('38123456789', 2, 'Camila', 'Torres', 'camila.torres@hotmail.com', '1133221100', '1988-03-20', TRUE),
('42923456789', 1, 'Mateo', 'Vidal', 'mateo.vidal@yahoo.com.ar', '2614567890', '1995-07-15', TRUE),
('45623456789', 2, 'Sofía', 'Ruiz', 'sofia.ruiz@live.com.ar', '3415678901', '1998-02-28', TRUE),
('49023456789', 1, 'Marcos', 'Díaz', 'marcos.diaz@outlook.com', '1567890123', '1987-09-10', TRUE);

INSERT INTO nutritionists (cuit, name, lastname, email, phone, enabled, created_at) VALUES
('35723456789', 'Nut. Mariana', 'Silva', 'mariana.silva@gmail.com', '1558743210', TRUE, '2021-08-12'),
('38123456789', 'Nut. Federico', 'Castro', 'federico.castro@hotmail.com', '1133221100', TRUE, '2022-04-05'),
('42923456789', 'Nut. Julieta', 'Martínez', 'julieta.martinez@yahoo.com.ar', '2614567890', TRUE, '2023-01-20'),
('45623456789', 'Nut. Pablo', 'Romero', 'pablo.romero@live.com.ar', '3415678901', TRUE, '2022-11-15'),
('49023456789', 'Nut. Sofía', 'Alvarez', 'sofia.alvarez@outlook.com', '1567890123', TRUE, '2023-05-08');

INSERT INTO routines (goals, created_at, trainer_id, client_id, active) VALUES
('Perder 5 kg en 3 meses', '2022-01-01', 1, 1, FALSE),
('Ganar masa muscular en 6 meses', '2022-01-15', 2, 2, FALSE),
('Mejorar la resistencia cardiovascular en 2 meses', '2022-02-01', 3, 3, FALSE),
('Reducir el porcentaje de grasa corporal en 4 meses', '2022-03-01', 4, 4, FALSE),
('Aumentar la fuerza muscular en 5 meses', '2022-04-01', 5, 5, FALSE);

INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES
(10, 3, 30, 1, 1),
(12, 4, 45, 1, 2),
(8, 2, 60, 1, 3),
(15, 5, 30, 2, 4),
(10, 3, 45, 2, 5),
(12, 4, 60, 3, 6),
(8, 2, 30, 3, 7),
(15, 5, 45, 4, 8),
(10, 3, 60, 4, 9),
(12, 4, 30, 5, 10);

INSERT INTO routines (goals, created_at, trainer_id, client_id, active) VALUES
('Aumentar la fuerza muscular en las piernas', '2022-02-01', 2, 2, FALSE),
('Mejorar la resistencia cardiovascular en 3 meses', '2022-02-15', 3, 3, FALSE),
('Reducir el porcentaje de grasa corporal en 4 meses', '2022-03-01', 4, 4, FALSE),
('Aumentar la flexibilidad y movilidad en 2 meses', '2022-03-15', 5, 5, FALSE),
('Mejorar la técnica de levantamiento de pesas', '2022-04-01', 1, 1, FALSE);

INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES
(15, 5, 30, 2, 1),
(10, 3, 45, 2, 4),
(12, 4, 60, 2, 5),
(8, 2, 30, 3, 6),
(12, 4, 45, 3, 7),
(10, 3, 60, 4, 8),
(15, 5, 30, 4, 9),
(8, 2, 45, 5, 10),
(12, 4, 60, 5, 11),
(10, 3, 30, 1, 12);

INSERT INTO routines (goals, created_at, trainer_id, client_id, active) VALUES
('Mejorar la fuerza muscular en el pecho', '2022-03-01', 3, 3, FALSE),
('Aumentar la resistencia cardiovascular en 4 meses', '2022-03-15', 4, 4, FALSE),
('Reducir el porcentaje de grasa corporal en 5 meses', '2022-04-01', 5, 5, FALSE),
('Aumentar la flexibilidad y movilidad en 3 meses', '2022-04-15', 1, 1, FALSE),
('Mejorar la técnica de levantamiento de pesas en 2 meses', '2022-05-01', 2, 2, FALSE);

INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES
(8, 2, 30, 3, 2),
(12, 4, 45, 3, 6),
(10, 3, 60, 3, 7),
(15, 5, 30, 4, 8),
(10, 3, 45, 4, 9),
(12, 4, 60, 5, 10),
(8, 2, 30, 5, 11),
(15, 5, 45, 1, 12),
(10, 3, 60, 1, 13),
(12, 4, 30, 2, 14);

INSERT INTO routines (goals, created_at, trainer_id, client_id, active) VALUES
('Aumentar la fuerza muscular en los brazos', '2022-04-01', 4, 4, FALSE),
('Mejorar la resistencia cardiovascular en 5 meses', '2022-04-15', 5, 5, FALSE),
('Reducir el porcentaje de grasa corporal en 6 meses', '2022-05-01', 1, 1, FALSE),
('Aumentar la flexibilidad y movilidad en 4 meses', '2022-05-15', 2, 2, FALSE),
('Mejorar la técnica de levantamiento de pesas en 3 meses', '2022-06-01', 3, 3, FALSE);

INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES
(12, 4, 30, 4, 3),
(15, 5, 45, 4, 8),
(10, 3, 60, 4, 9),
(8, 2, 30, 5, 10),
(12, 4, 45, 5, 11),
(10, 3, 60, 1, 12),
(15, 5, 30, 1, 13),
(8, 2, 45, 2, 14),
(12, 4, 60, 2, 15),
(10, 3, 30, 3, 16);

INSERT INTO routines (goals, created_at, trainer_id, client_id, active) VALUES
('Aumentar la flexibilidad y movilidad en los hombros', '2022-05-01', 5, 5, TRUE),
('Mejorar la resistencia cardiovascular en 6 meses', '2022-05-15', 1, 1, TRUE),
('Reducir el porcentaje de grasa corporal en 7 meses', '2022-06-01', 2, 2, TRUE),
('Aumentar la fuerza muscular en los glúteos', '2022-06-15', 3, 3, TRUE),
('Mejorar la técnica de levantamiento de pesas en 4 meses', '2022-07-01', 4, 4, TRUE);

INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES
(10, 3, 30, 5, 4),
(12, 4, 45, 5, 10),
(8, 2, 60, 5, 11),
(15, 5, 30, 6, 12),
(10, 3, 45, 6, 13),
(12, 4, 60, 7, 14),
(8, 2, 30, 7, 15),
(15, 5, 45, 8, 16),
(10, 3, 60, 8, 17),
(12, 4, 30, 9, 18);

INSERT INTO training_diary (commentary, created_at, routine_id) VALUES ('Comentario 1', '2022-01-01 08:00:00', 1);
INSERT INTO training_diary (commentary, created_at, routine_id) VALUES ('Comentario 2', '2022-01-02 09:30:00', 1);
INSERT INTO training_diary (commentary, created_at, routine_id) VALUES ('Comentario 3', '2022-01-03 10:45:00', 1);
INSERT INTO training_diary (commentary, created_at, routine_id) VALUES ('Comentario 4', '2022-01-04 12:15:00', 1);
INSERT INTO training_diary (commentary, created_at, routine_id) VALUES ('Comentario 5', '2022-01-05 14:30:00', 1);

INSERT INTO training_diary (commentary, created_at, routine_id) VALUES ('Comentario 1', '2022-01-01 08:00:00', 2);
INSERT INTO training_diary (commentary, created_at, routine_id) VALUES ('Comentario 2', '2022-01-02 09:30:00', 2);
INSERT INTO training_diary (commentary, created_at, routine_id) VALUES ('Comentario 3', '2022-01-03 10:45:00', 2);
INSERT INTO training_diary (commentary, created_at, routine_id) VALUES ('Comentario 4', '2022-01-04 12:15:00', 2);
INSERT INTO training_diary (commentary, created_at, routine_id) VALUES ('Comentario 5', '2022-01-05 14:30:00', 2);

INSERT INTO nutrition_plan (created_at, daily_calories, daily_carbohydrates, daily_fats, daily_proteins, desired_weight, enabled, nutritionist_id, client_id) VALUES
('2022-01-01', 1800.0, 45.0, 25.0, 60.0, 65.0, TRUE, 1, 1),
('2022-01-05', 2200.0, 55.0, 35.0, 80.0, 75.0, TRUE, 2, 2),
('2022-01-10', 2000.0, 50.0, 30.0, 70.0, 70.0, TRUE, 3, 3),
('2022-01-15', 2500.0, 60.0, 40.0, 90.0, 80.0, TRUE, 4, 4),
('2022-01-20', 1500.0, 35.0, 20.0, 50.0, 60.0, TRUE, 5, 5);

INSERT INTO nutrition_diary (created_at, breakfast, lunch, snacks, dinner, actual_weight, commentary, nutrition_plan_id) VALUES
('2022-01-03 08:00:00', 'Avena con frutas y nueces', 'Ensalada de pollo con aguacate', 'Manzana y almendras', 'Salmón a la plancha con quinoa', 65.5, 'Me siento con energía después del desayuno', 1),
('2022-01-03 12:00:00', 'Huevos revueltos con espinacas', 'Sándwich de pavo con lechuga y tomate', 'Yogur griego con miel', 'Pollo al curry con arroz integral', 65.8, 'Me duele un poco la barriga después del almuerzo', 1),
('2022-01-04 08:00:00', 'Batido de proteínas con plátano y leche', 'Ensalada de atún con aguacate y huevo', 'Palitos de zanahoria con hummus', 'Cerdo a la plancha con puré de calabaza', 66.2, 'Me siento saciado después del desayuno', 1),
('2022-01-04 12:00:00', 'Tortilla de espinacas con tomate', 'Sopa de lentejas con pan integral', 'Frutas secas y nueces', 'Pollo al horno con patatas asadas', 66.5, 'Me siento con energía después del almuerzo', 1),
('2022-01-05 08:00:00', 'Avena con frutas y nueces', 'Ensalada de pollo con aguacate', 'Manzana y almendras', 'Salmón a la plancha con quinoa', 67.0, 'Me siento saciado después del desayuno', 1);

INSERT INTO nutrition_diary (created_at, breakfast, lunch, snacks, dinner, actual_weight, commentary, nutrition_plan_id) VALUES
('2022-01-06 08:00:00', 'Batido de proteínas con espinacas y banana', 'Ensalada de quinoa con pollo a la parrilla', 'Manzana y almendras', 'Bistec a la plancha con vegetales salteados', 74.8, 'Me siento con mucha energía hoy', 2),
('2022-01-06 12:00:00', 'Huevos revueltos con champiñones', 'Sándwich de pavo en pan integral con aguacate', 'Yogur griego con frutos rojos', 'Salmón al horno con arroz integral', 75.0, '¡Logré mi peso objetivo!', 2),
('2022-01-07 08:00:00', 'Avena con frutas y semillas de chía', 'Sopa de lentejas con pan integral', 'Zanahoria y hummus', 'Pollo al curry con arroz integral', 74.9, 'Me siento un poco pesado después del almuerzo', 2),
('2022-01-07 12:00:00', 'Tortilla de verduras', 'Ensalada de atún con aguacate', 'Frutos secos mixtos', 'Cerdo a la plancha con puré de batata', 75.1, 'Estoy muy satisfecho con mi dieta', 2),
('2022-01-08 08:00:00', 'Batido de proteínas con plátano y leche de almendras', 'Ensalada de pollo con quinoa', 'Manzana y almendras', 'Bistec a la plancha con vegetales salteados', 74.7, 'Me siento genial después del entrenamiento', 2);
