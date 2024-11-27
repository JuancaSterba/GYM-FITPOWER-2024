INSERT INTO trainers (cuit, name, lastname, email, phone_number, created_at, enabled) VALUES ('20123456783', 'Juan', 'Pérez', 'juan.perez@example.com', '1234567890', '2022-01-01', TRUE);

INSERT INTO trainers (cuit, name, lastname, email, phone_number, created_at, enabled) VALUES ('27234567892', 'María', 'Gómez', 'maria.gomez@example.com', '9876543210', '2022-02-01', TRUE);

INSERT INTO trainers (cuit, name, lastname, email, phone_number, created_at, enabled) VALUES ('30345678909', 'Pedro', 'Rodríguez', 'pedro.rodriguez@example.com', '5555555555', '2022-03-01', TRUE);

INSERT INTO trainers (cuit, name, lastname, email, phone_number, created_at, enabled) VALUES ('33456789015', 'Ana', 'Díaz', 'ana.diaz@example.com', '6666666666', '2022-04-01', TRUE);

INSERT INTO trainers (cuit, name, lastname, email, phone_number, created_at, enabled) VALUES ('20567890125', 'Luis', 'Martínez', 'luis.martinez@example.com', '7777777777', '2022-05-01', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 1', 'Descripción del ejercicio 1', 'Grupo muscular 1', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 2', 'Descripción del ejercicio 2', 'Grupo muscular 1', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 3', 'Descripción del ejercicio 3', 'Grupo muscular 2', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 4', 'Descripción del ejercicio 4', 'Grupo muscular 2', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 5', 'Descripción del ejercicio 5', 'Grupo muscular 3', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 6', 'Descripción del ejercicio 6', 'Grupo muscular 3', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 7', 'Descripción del ejercicio 7', 'Grupo muscular 4', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 8', 'Descripción del ejercicio 8', 'Grupo muscular 4', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 9', 'Descripción del ejercicio 9', 'Grupo muscular 5', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 10', 'Descripción del ejercicio 10', 'Grupo muscular 5', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 11', 'Descripción del ejercicio 11', 'Grupo muscular 6', TRUE);

INSERT INTO gyms (address, domain, mail, phone, enabled) VALUES ('Calle 123', 'gimnasio.com', 'gimnasio@example', '1234567890', TRUE);

INSERT INTO gyms (address, domain, mail, phone, enabled) VALUES ('Calle 456', 'gimnasio2.com', 'gimnasio2@example', '9876543210', TRUE);

INSERT INTO gyms (address, domain, mail, phone, enabled) VALUES ('Calle 789', 'gimnasio3.com', 'gimnasio3@example', '5555555555', TRUE);

INSERT INTO clients (cuit, assigned_gym_id, name, lastname, email, phone, birth_date, enabled) VALUES ('20123456783', 1, 'Cliente 1', 'Pérez', 'cliente1@example', '1234567890', '1990-01-01', TRUE);

INSERT INTO clients (cuit, assigned_gym_id, name, lastname, email, phone, birth_date, enabled) VALUES ('27234567892', 1, 'Cliente 2', 'Gómez', 'cliente2@example', '9876543210', '1995-02-01', TRUE);

INSERT INTO clients (cuit, assigned_gym_id, name, lastname, email, phone, birth_date, enabled) VALUES ('30345678909', 2, 'Cliente 3', 'Rodriguez', 'cliente3@example', '5555555555', '1980-03-01', TRUE);

INSERT INTO clients (cuit, assigned_gym_id, name, lastname, email, phone, birth_date, enabled) VALUES ('33456789015', 2, 'Cliente 4', 'Díaz', 'cliente4@example', '6666666666', '1985-04-01', TRUE);

INSERT INTO clients (cuit, assigned_gym_id, name, lastname, email, phone, birth_date, enabled) VALUES ('20567890125', 3, 'Cliente 5', 'Martínez', 'cliente5@example', '7777777777', '1970-05-01', TRUE);

INSERT INTO nutritionists (cuit, name, lastname, email, phone, enabled, created_at) VALUES ('20123456783', 'Nutricionista 1', 'Pérez', 'nutricionista1@example', '1234567890', TRUE, '2022-01-01');

INSERT INTO nutritionists (cuit, name, lastname, email, phone, enabled, created_at) VALUES ('27234567892', 'Nutricionista 2', 'Gómez', 'nutricionista2@example', '9876543210', TRUE, '2022-02-01');

INSERT INTO nutritionists (cuit, name, lastname, email, phone, enabled, created_at) VALUES ('30345678909', 'Nutricionista 3', 'Rodriguez', 'nutricionista3@example', '5555555555', TRUE, '2022-03-01');

INSERT INTO nutritionists (cuit, name, lastname, email, phone, enabled, created_at) VALUES ('33456789015', 'Nutricionista 4', 'Díaz', 'nutricionista4@example', '6666666666', TRUE, '2022-04-01');

INSERT INTO nutritionists (cuit, name, lastname, email, phone, enabled, created_at) VALUES ('20567890125', 'Nutricionista 5', 'Martínez', 'nutricionista5@example', '7777777777', TRUE, '2022-05-01');

INSERT INTO routines (goals, created_at, trainer_id, client_id, active) VALUES ('Objetivo 1', '2022-01-01', 1, 1, TRUE);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (10, 3, 30, 1, 1);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (12, 4, 45, 1, 2);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (8, 2, 60, 1, 3);

INSERT INTO routines (goals, created_at, trainer_id, client_id, active) VALUES ('Objetivo 2', '2022-02-01', 2, 2, TRUE);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (15, 5, 30, 2, 1);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (10, 3, 45, 2, 4);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (12, 4, 60, 2, 5);

INSERT INTO routines (goals, created_at, trainer_id, client_id, active) VALUES ('Objetivo 3', '2022-03-01', 3, 3, TRUE);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (8, 2, 30, 3, 2);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (12, 4, 45, 3, 6);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (10, 3, 60, 3, 7);

INSERT INTO routines (goals, created_at, trainer_id, client_id, active) VALUES ('Objetivo 4', '2022-04-01', 4, 4, TRUE);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (12, 4, 30, 4, 3);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (15, 5, 45, 4, 8);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (10, 3, 60, 4, 9);

INSERT INTO routines (goals, created_at, trainer_id, client_id, active) VALUES ('Objetivo 5', '2022-05-01', 5, 5, TRUE);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (10, 3, 30, 5, 4);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (12, 4, 45, 5, 10);
INSERT INTO exercise_sets (reps, sets, rest_in_minutes, routine_id, exercise_id) VALUES (8, 2, 60, 5, 11);

INSERT INTO training_diary (comentary, created_at, routine_id) VALUES ('Comentario 1', '2022-01-01 08:00:00', 1);
INSERT INTO training_diary (comentary, created_at, routine_id) VALUES ('Comentario 2', '2022-01-02 09:30:00', 1);
INSERT INTO training_diary (comentary, created_at, routine_id) VALUES ('Comentario 3', '2022-01-03 10:45:00', 1);
INSERT INTO training_diary (comentary, created_at, routine_id) VALUES ('Comentario 4', '2022-01-04 12:15:00', 1);
INSERT INTO training_diary (comentary, created_at, routine_id) VALUES ('Comentario 5', '2022-01-05 14:30:00', 1);

INSERT INTO training_diary (comentary, created_at, routine_id) VALUES ('Comentario 1', '2022-01-01 08:00:00', 2);
INSERT INTO training_diary (comentary, created_at, routine_id) VALUES ('Comentario 2', '2022-01-02 09:30:00', 2);
INSERT INTO training_diary (comentary, created_at, routine_id) VALUES ('Comentario 3', '2022-01-03 10:45:00', 2);
INSERT INTO training_diary (comentary, created_at, routine_id) VALUES ('Comentario 4', '2022-01-04 12:15:00', 2);
INSERT INTO training_diary (comentary, created_at, routine_id) VALUES ('Comentario 5', '2022-01-05 14:30:00', 2);