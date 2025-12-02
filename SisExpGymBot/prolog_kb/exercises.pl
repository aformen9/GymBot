% ----------------------------------------------------------
% exercises.pl  - Catálogo EXPANDIDO con sistema de lesiones mejorado
% Formato: exercise(Nombre, Grupo, Objetivo, Nivel, Equipamiento, LesionContra).
% ----------------------------------------------------------

% --- PECHO ---
% Ganar masa
exercise(press_banca_plano, pecho, ganar_masa, principiante, gym_completo, ninguna).
exercise(press_banca_inclinado, pecho, ganar_masa, intermedio, gym_completo, ninguna).
exercise(press_banca_declinado, pecho, ganar_masa, intermedio, gym_completo, extremidad_superior).
exercise(aperturas_mancuernas, pecho, ganar_masa, principiante, mancuernas, ninguna).
exercise(aperturas_inclinadas, pecho, ganar_masa, intermedio, mancuernas, extremidad_superior).
exercise(flexiones_lastradas, pecho, ganar_masa, principiante, peso_corporal, ninguna).
exercise(flexiones_diamante, pecho, ganar_masa, principiante, peso_corporal, extremidad_superior).
exercise(flexiones_arquero, pecho, ganar_masa, avanzado, peso_corporal, extremidad_superior).
exercise(press_maquina_pecho, pecho, ganar_masa, principiante, gym_completo, ninguna).

% Bajar grasa
exercise(flexiones_suelo, pecho, bajar_grasa, principiante, peso_corporal, ninguna).
exercise(flexiones_inclinadas, pecho, bajar_grasa, principiante, peso_corporal, extremidad_superior).
exercise(burpees, pecho, bajar_grasa, intermedio, peso_corporal, extremidad_inferior).
exercise(flexiones_explosivas, pecho, bajar_grasa, avanzado, peso_corporal, extremidad_superior).

% Fuerza
exercise(press_banca_pesado, pecho, fuerza, intermedio, gym_completo, extremidad_superior).
exercise(press_banca_cerrado, pecho, fuerza, intermedio, gym_completo, extremidad_superior).
exercise(press_banca_barra, pecho, fuerza, principiante, gym_completo, extremidad_superior).
exercise(contractora_pecho, pecho, ganar_masa, principiante, gym_completo, ninguna).

% --- ESPALDA ---
% Ganar masa
exercise(remo_barra, espalda, ganar_masa, intermedio, gym_completo, espalda_lumbar).
exercise(remo_mancuerna, espalda, ganar_masa, principiante, mancuernas, ninguna).
exercise(jalon_pecho, espalda, ganar_masa, principiante, gym_completo, ninguna).
exercise(remo_invertido, espalda, ganar_masa, principiante, peso_corporal, espalda_lumbar).
exercise(pullover_mancuernas, espalda, ganar_masa, intermedio, mancuernas, extremidad_superior).
exercise(remo_polea_baja, espalda, ganar_masa, principiante, gym_completo, espalda_lumbar).
exercise(dominadas_asistidas, espalda, ganar_masa, principiante, gym_completo, extremidad_superior).
exercise(remo_maquina, espalda, ganar_masa, principiante, gym_completo, ninguna).

% Bajar grasa
exercise(remo_alto_cuerda, espalda, bajar_grasa, principiante, gym_completo, ninguna).
exercise(superman_suelo, espalda, bajar_grasa, principiante, peso_corporal, espalda_lumbar).

% Fuerza
exercise(peso_muerto, espalda, fuerza, intermedio, gym_completo, espalda_lumbar).
exercise(peso_muerto_sumo, espalda, fuerza, intermedio, gym_completo, espalda_lumbar).
exercise(dominadas, espalda, fuerza, avanzado, peso_corporal, extremidad_superior).
exercise(dominadas_lastradas, espalda, fuerza, avanzado, gym_completo, extremidad_superior).

% --- PIERNAS ---
% Peso corporal / Mancuernas
exercise(sentadilla_aire, piernas, ganar_masa, principiante, peso_corporal, extremidad_inferior).
exercise(sentadilla_bulgara, piernas, ganar_masa, intermedio, peso_corporal, extremidad_inferior).
exercise(puente_gluteos, piernas, ganar_masa, principiante, peso_corporal, ninguna).
exercise(puente_una_pierna, piernas, ganar_masa, intermedio, peso_corporal, ninguna).
exercise(zancadas_mancuernas, piernas, ganar_masa, principiante, mancuernas, extremidad_inferior).
exercise(zancadas_caminando, piernas, ganar_masa, intermedio, mancuernas, extremidad_inferior).
exercise(peso_muerto_rumano, piernas, ganar_masa, intermedio, mancuernas, espalda_lumbar).
exercise(goblet_squat, piernas, ganar_masa, principiante, mancuernas, extremidad_inferior).

% Gym Completo
exercise(sentadilla_libre, piernas, ganar_masa, intermedio, gym_completo, extremidad_inferior).
exercise(prensa_45, piernas, ganar_masa, principiante, gym_completo, extremidad_inferior).
exercise(extension_cuadriceps, piernas, ganar_masa, principiante, gym_completo, extremidad_inferior).
exercise(curl_femoral, piernas, ganar_masa, principiante, gym_completo, ninguna).
exercise(sentadilla_frontal, piernas, fuerza, avanzado, gym_completo, extremidad_inferior).
exercise(hip_thrust_barra, piernas, ganar_masa, intermedio, gym_completo, ninguna).

% Bajar grasa
exercise(sentadilla_salto, piernas, bajar_grasa, principiante, peso_corporal, extremidad_inferior).
exercise(step_up_banco, piernas, bajar_grasa, principiante, peso_corporal, extremidad_inferior).
exercise(zancadas_salto, piernas, bajar_grasa, intermedio, peso_corporal, extremidad_inferior).
exercise(skater_jumps, piernas, bajar_grasa, intermedio, peso_corporal, extremidad_inferior).

% Fuerza
exercise(sentadilla_profunda, piernas, fuerza, avanzado, gym_completo, extremidad_inferior).
exercise(peso_muerto_convencional, piernas, fuerza, intermedio, gym_completo, espalda_lumbar).

% --- HOMBROS ---
% Gym / Mancuernas
exercise(press_militar_barra, hombros, ganar_masa, intermedio, gym_completo, extremidad_superior).
exercise(elevaciones_laterales, hombros, ganar_masa, principiante, gym_completo, extremidad_superior).
exercise(elevaciones_frontales, hombros, ganar_masa, principiante, gym_completo, extremidad_superior).
exercise(pajaros_mancuernas, hombros, ganar_masa, principiante, mancuernas, extremidad_superior).
exercise(face_pull, hombros, ganar_masa, principiante, gym_completo, ninguna).
exercise(press_mancuernas, hombros, ganar_masa, principiante, mancuernas, extremidad_superior).
exercise(press_arnold, hombros, ganar_masa, intermedio, mancuernas, extremidad_superior).
exercise(pike_push_up, hombros, ganar_masa, intermedio, peso_corporal, extremidad_superior).

% Bajar grasa / Fuerza
exercise(circulos_brazos, hombros, bajar_grasa, principiante, peso_corporal, extremidad_superior).
exercise(press_militar_pesado, hombros, fuerza, intermedio, gym_completo, extremidad_superior).
exercise(push_press, hombros, fuerza, avanzado, gym_completo, extremidad_superior).

% --- CORE ---
exercise(crunch_abdominal, core, ganar_masa, principiante, peso_corporal, espalda_lumbar).
exercise(crunch_inverso, core, ganar_masa, principiante, peso_corporal, espalda_lumbar).
exercise(elevacion_piernas, core, ganar_masa, intermedio, gym_completo, espalda_lumbar).
exercise(plancha_abdominal, core, bajar_grasa, principiante, peso_corporal, ninguna).
exercise(plancha_lateral, core, bajar_grasa, principiante, peso_corporal, extremidad_superior).
exercise(mountain_climbers, core, bajar_grasa, principiante, peso_corporal, extremidad_superior).
exercise(bicicleta_abdominal, core, bajar_grasa, principiante, peso_corporal, ninguna).
exercise(russian_twist, core, ganar_masa, intermedio, peso_corporal, espalda_lumbar).
exercise(rollout_rueda, core, fuerza, intermedio, gym_completo, espalda_lumbar).
exercise(toes_to_bar, core, fuerza, avanzado, gym_completo, extremidad_superior).
exercise(dragon_flag, core, fuerza, avanzado, gym_completo, espalda_lumbar).
exercise(dead_bug, core, ganar_masa, principiante, peso_corporal, ninguna).