% ----------------------------------------------------------
% exercises.pl  - Catálogo de ejercicios
% exercise(Nombre, Grupo, Objetivo, Munecain, Equipamiento, LesionContra)
% ----------------------------------------------------------


% Ganar masa
exercise(Press_banca_plano, Pecho, Ganar_masa, Principiante, Gym_completo, Ninguna).
exercise(Press_banca_inclinado, Pecho, Ganar_masa, Intermedio, Gym_completo, Ninguna).
exercise(Aperturas_mancuernas_plano, Pecho, Ganar_masa, Principiante, Mancuernas, Ninguna).
exercise(Flexiones_lastradas, Pecho, Ganar_masa, Principiante, Peso_corporal, Ninguna).
exercise(Flexiones_diamante, Pecho, Ganar_masa, Principiante, Peso_corporal, Muneca).

% -------- PECHO --------

% Bajar grasa
exercise(Flexiones_suelo, Pecho, Bajar_grasa, Principiante, Peso_corporal, Ninguna).
exercise(Flexiones_inclinadas, Pecho, Bajar_grasa, Principiante, Peso_corporal, Muneca).

% Fuerza
exercise(Press_banca_pesado, Pecho, Fuerza, Intermedio, Gym_completo, Hombro).

% Gym completo
exercise(press_banca_plano_barra, pecho, ganar_masa, principiante, gym_completo, hombro).
exercise(press_banca_inclinado_barra, pecho, ganar_masa, intermedio, gym_completo, hombro).
exercise(contractora_pecho, pecho, ganar_masa, principiante, gym_completo, ninguna).

% -------- ESPALDA --------

% Ganar masa
exercise(Remo_barra, Espalda, Ganar_masa, Intermedio, Gym_completo, Espalda).
exercise(Remo_mancuerna, Espalda, Ganar_masa, Principiante, Mancuernas, Ninguna).
exercise(Jalon_pecho, Espalda, Ganar_masa, Principiante, Gym_completo, Ninguna).
exercise(Remador_peso_corporal, Espalda, Ganar_masa, Principiante, Peso_corporal, Espalda).

% Mancuernas
exercise(remo_mancuernas, espalda, ganar_masa, principiante, mancuernas, espalda).
exercise(pullover_mancuernas, espalda, ganar_masa, intermedio, mancuernas, hombro).

% Bajar grasa
exercise(Remador_en_maquina, Espalda, Bajar_grasa, Principiante, Gym_completo, Espalda).
exercise(Superman_suelo, Espalda, Bajar_grasa, Principiante, Peso_corporal, Espalda).

% Fuerza
exercise(Peso_muerto_convencional, Espalda, Fuerza, Intermedio, Gym_completo, Espalda).

% Peso corporal
exercise(sentadilla_peso_corporal, piernas, ganar_masa, principiante, peso_corporal, rodilla).
exercise(puente_gluteos, piernas, ganar_masa, principiante, peso_corporal, ninguna).

% Mancuernas
exercise(zancadas_mancuernas, piernas, ganar_masa, principiante, mancuernas, rodilla).
exercise(peso_muerto_rumano_mancuernas, piernas, ganar_masa, intermedio, mancuernas, espalda).

% Ganar masa
exercise(Sentadilla_libre, Piernas, Ganar_masa, Intermedio, Gym_completo, Rodilla).
exercise(Prensa_45, Piernas, Ganar_masa, Principiante, Gym_completo, Rodilla).
exercise(Zancadas_mancuernas, Piernas, Ganar_masa, Intermedio, Mancuernas, Rodilla).
exercise(Sentadilla_peso_corporal, Piernas, Ganar_masa, Principiante, Peso_corporal, Rodilla).
exercise(Sentadilla_balgariana_peso_corporal, Piernas, Ganar_masa, Principiante, Peso_corporal, Rodilla).

% -------- HOMBROS --------

% Bajar grasa
exercise(Sentadilla_sin_peso, Piernas, Bajar_grasa, Principiante, Peso_corporal, Rodilla).
exercise(Step_up_banco, Piernas, Bajar_grasa, Principiante, Peso_corporal, Rodilla).

% Fuerza
exercise(Sentadilla_frontal_pesada, Piernas, Fuerza, Avanzado, Gym_completo, Rodilla).

% Gym completo
exercise(press_militar_barra, hombros, ganar_masa, intermedio, gym_completo, hombro).
exercise(elevaciones_laterales_maquina, hombros, ganar_masa, principiante, gym_completo, hombro).
exercise(face_pull_polea, hombros, ganar_masa, principiante, gym_completo, ninguna).

% -------- CORE --------

% Ganar masa
exercise(Press_militar_mancuernas, Hombros, Ganar_masa, Principiante, Mancuernas, Ninguna).
exercise(Press_militar_barra, Hombros, Ganar_masa, Intermedio, Gym_completo, Hombro).
exercise(Elevaciones_laterales, Hombros, Ganar_masa, Principiante, Mancuernas, Hombro).
exercise(Pike_push_up, Hombros, Ganar_masa, Intermedio, Peso_corporal, Hombro).

% -------- HOMBROS --------

% Bajar grasa
exercise(Circulos_brazos, Hombros, Bajar_grasa, Principiante, Peso_corporal, Hombro).

% Fuerza
exercise(Press_militar_de_pie, Hombros, Fuerza, Intermedio, Gym_completo, Hombro).

% Gym completo
exercise(press_militar_barra_pesado, hombros, fuerza, intermedio, gym_completo, hombro).

% -------- CORE --------

% Ganar masa
exercise(Crunch_abdominal, Core, Ganar_masa, Principiante, Peso_corporal, Espalda).
exercise(Elevaciones_piernas_colgado, Core, Ganar_masa, Intermedio, Gym_completo, Espalda).

% Bajar grasa
exercise(Plancha_abdominal, Core, Bajar_grasa, Principiante, Peso_corporal, Ninguna).
exercise(Mountain_climbers, Core, Bajar_grasa, Principiante, Peso_corporal, Muneca).

% Fuerza
exercise(Rollout_rueda, Core, Fuerza, Intermedio, Gym_completo, Espalda).
