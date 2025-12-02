% ----------------------------------------------------------
% exercises.pl  - Catálogo de ejercicios NORMALIZADO
% Formato: exercise(Nombre, Grupo, Objetivo, Nivel, Equipamiento, LesionContra).
% IMPORTANTE: Todo en minúsculas para evitar Singletons.
% ----------------------------------------------------------

% --- PECHO ---
% Ganar masa
exercise(press_banca_plano, pecho, ganar_masa, principiante, gym_completo, ninguna).
exercise(press_banca_inclinado, pecho, ganar_masa, intermedio, gym_completo, ninguna).
exercise(aperturas_mancuernas, pecho, ganar_masa, principiante, mancuernas, ninguna).
exercise(flexiones_lastradas, pecho, ganar_masa, principiante, peso_corporal, ninguna).
exercise(flexiones_diamante, pecho, ganar_masa, principiante, peso_corporal, muneca).

% Bajar grasa
exercise(flexiones_suelo, pecho, bajar_grasa, principiante, peso_corporal, ninguna).
exercise(flexiones_inclinadas, pecho, bajar_grasa, principiante, peso_corporal, muneca).
exercise(burpees, pecho, bajar_grasa, intermedio, peso_corporal, rodilla).

% Fuerza
exercise(press_banca_pesado, pecho, fuerza, intermedio, gym_completo, hombro).

% Gym completo / Variantes
exercise(press_banca_barra, pecho, ganar_masa, principiante, gym_completo, hombro).
exercise(contractora_pecho, pecho, ganar_masa, principiante, gym_completo, ninguna).

% --- ESPALDA ---
% Ganar masa
exercise(remo_barra, espalda, ganar_masa, intermedio, gym_completo, espalda).
exercise(remo_mancuerna, espalda, ganar_masa, principiante, mancuernas, ninguna).
exercise(jalon_pecho, espalda, ganar_masa, principiante, gym_completo, ninguna).
exercise(remo_invertido, espalda, ganar_masa, principiante, peso_corporal, espalda).
exercise(pullover_mancuernas, espalda, ganar_masa, intermedio, mancuernas, hombro).

% Bajar grasa
exercise(remo_maquina, espalda, bajar_grasa, principiante, gym_completo, espalda).
exercise(superman_suelo, espalda, bajar_grasa, principiante, peso_corporal, espalda).

% Fuerza
exercise(peso_muerto, espalda, fuerza, intermedio, gym_completo, espalda).
exercise(dominadas, espalda, fuerza, avanzado, peso_corporal, hombro).

% --- PIERNAS ---
% Peso corporal / Mancuernas
exercise(sentadilla_aire, piernas, ganar_masa, principiante, peso_corporal, rodilla).
exercise(puente_gluteos, piernas, ganar_masa, principiante, peso_corporal, ninguna).
exercise(zancadas_mancuernas, piernas, ganar_masa, principiante, mancuernas, rodilla).
exercise(peso_muerto_rumano, piernas, ganar_masa, intermedio, mancuernas, espalda).

% Gym Completo (CORREGIDO: Estaba en mayúsculas)
exercise(sentadilla_libre, piernas, ganar_masa, intermedio, gym_completo, rodilla).
exercise(prensa_45, piernas, ganar_masa, principiante, gym_completo, rodilla).
exercise(extension_cuadriceps, piernas, ganar_masa, principiante, gym_completo, rodilla).
exercise(sentadilla_frontal, piernas, fuerza, avanzado, gym_completo, rodilla).

% Bajar grasa
exercise(sentadilla_salto, piernas, bajar_grasa, principiante, peso_corporal, rodilla).
exercise(step_up_banco, piernas, bajar_grasa, principiante, peso_corporal, rodilla).

% --- HOMBROS ---
% Gym / Mancuernas
exercise(press_militar_barra, hombros, ganar_masa, intermedio, gym_completo, hombro).
exercise(elevaciones_laterales, hombros, ganar_masa, principiante, gym_completo, hombro).
exercise(face_pull, hombros, ganar_masa, principiante, gym_completo, ninguna).
exercise(press_mancuernas, hombros, ganar_masa, principiante, mancuernas, ninguna).
exercise(pike_push_up, hombros, ganar_masa, intermedio, peso_corporal, hombro).

% Bajar grasa / Fuerza
exercise(circulos_brazos, hombros, bajar_grasa, principiante, peso_corporal, hombro).
exercise(press_militar_pesado, hombros, fuerza, intermedio, gym_completo, hombro).

% --- CORE ---
exercise(crunch_abdominal, core, ganar_masa, principiante, peso_corporal, espalda).
exercise(elevacion_piernas, core, ganar_masa, intermedio, gym_completo, espalda).
exercise(plancha_abdominal, core, bajar_grasa, principiante, peso_corporal, ninguna).
exercise(mountain_climbers, core, bajar_grasa, principiante, peso_corporal, muneca).
exercise(rollout_rueda, core, fuerza, intermedio, gym_completo, espalda).