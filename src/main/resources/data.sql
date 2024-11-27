INSERT INTO trainers (cuit, name, lastname, email, phone_number, created_at, enabled) VALUES ('20123456783', 'Juan', 'Pérez', 'juan.perez@example.com', '1234567890', '2022-01-01', TRUE);

INSERT INTO trainers (cuit, name, lastname, email, phone_number, created_at, enabled) VALUES ('27234567892', 'María', 'Gómez', 'maria.gomez@example.com', '9876543210', '2022-02-01', TRUE);

INSERT INTO trainers (cuit, name, lastname, email, phone_number, created_at, enabled) VALUES ('30345678909', 'Pedro', 'Rodríguez', 'pedro.rodriguez@example.com', '5555555555', '2022-03-01', TRUE);

INSERT INTO trainers (cuit, name, lastname, email, phone_number, created_at, enabled) VALUES ('33456789015', 'Ana', 'Díaz', 'ana.diaz@example.com', '6666666666', '2022-04-01', TRUE);

INSERT INTO trainers (cuit, name, lastname, email, phone_number, created_at, enabled) VALUES ('20567890125', 'Luis', 'Martínez', 'luis.martinez@example.com', '7777777777', '2022-05-01', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 1', 'Descripción del ejercicio 1', 'Grupo muscular 1', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 2', 'Descripción del ejercicio 2', 'Grupo muscular 1', TRUE);

INSERT INTO exercises (name, description, muscle_group, enabled) VALUES ('Ejercicio 3', 'Descripción del ejercicio 3', 'Grupo muscular 2', TRUE);

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