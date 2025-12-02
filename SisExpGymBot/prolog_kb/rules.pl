% ----------------------------------------------------------
% rules.pl  - Motor experto ROBUSTO (Con Fallback)
% ----------------------------------------------------------

:- discontiguous exercise/6.

% ===========================
% NIVELES
% ===========================

nivel_orden(principiante, 1).
nivel_orden(intermedio,  2).
nivel_orden(avanzado,    3).

nivel_suficiente(NivelUsuario, NivelMin) :-
    nivel_orden(NivelUsuario, NU),
    nivel_orden(NivelMin, NM),
    NU >= NM.

% ===========================
% EQUIPAMIENTO
% ===========================

% gym_completo incluye todo
equip_compatible(gym_completo, _).
% Si tengo mancuernas, también tengo mi cuerpo
equip_compatible(mancuernas, peso_corporal). 
equip_compatible(E, E).

% ===========================
% LESIONES
% ===========================

compatible_lesion(LesionUsuario, LesionContra) :-
    LesionContra == ninguna ;
    LesionUsuario \= LesionContra.

% ===========================
% SELECCIÓN DE EJERCICIOS (NÚCLEO)
% ===========================

% 1. Coincidencia EXACTA
ejercicio_apto(Objetivo, Nivel, Equip, Lesion, Grupo, Nombre) :-
    exercise(Nombre, Grupo, Objetivo, NivelMin, EquipReq, LesionContra),
    nivel_suficiente(Nivel, NivelMin),
    equip_compatible(Equip, EquipReq),
    compatible_lesion(Lesion, LesionContra).

% 2. FALLBACK (Plan B): Si no hay exacto, usar 'ganar_masa'
% Esto evita que la rutina venga vacía si faltan ejercicios específicos
ejercicio_apto(Objetivo, Nivel, Equip, Lesion, Grupo, Nombre) :-
    Objetivo \= ganar_masa,
    exercise(Nombre, Grupo, ganar_masa, NivelMin, EquipReq, LesionContra),
    nivel_suficiente(Nivel, NivelMin),
    equip_compatible(Equip, EquipReq),
    compatible_lesion(Lesion, LesionContra).

% ===========================
% VARIACIÓN
% ===========================

lista_ejercicios_aptos(O,N,E,L,Grupo,Lista) :-
    findall(Nombre, ejercicio_apto(O,N,E,L,Grupo,Nombre), ListaBruta),
    list_to_set(ListaBruta, Lista). % Elimina duplicados del fallback

elige_ejercicio_por_indice(O,N,E,L,Grupo,Dia,Nombre) :-
    lista_ejercicios_aptos(O,N,E,L,Grupo,Lista),
    Lista \= [], % Si esto falla, Prolog devuelve "false" y la rutina sale vacía
    length(Lista, Len),
    I0 is (Dia - 1) mod Len,
    nth0(I0, Lista, Nombre).

% ===========================
% RUTINAS CON VARIACIÓN POR DÍA
% ===========================

% ----- FULLBODY -----
rutina_fullbody_dia(O,N,E,L,Dia, [
    [pecho,   E1],
    [espalda, E2],
    [piernas, E3],
    [hombros, E4],
    [core,    E5]
]) :-
    elige_ejercicio_por_indice(O,N,E,L, pecho,   Dia, E1),
    elige_ejercicio_por_indice(O,N,E,L, espalda, Dia, E2),
    elige_ejercicio_por_indice(O,N,E,L, piernas, Dia, E3),
    elige_ejercicio_por_indice(O,N,E,L, hombros, Dia, E4),
    elige_ejercicio_por_indice(O,N,E,L, core,    Dia, E5).

% ----- PUSH -----
rutina_push_dia(O,N,E,L,Dia, [
    [pecho,   E1],
    [hombros, E2],
    [core,    E3]
]) :-
    elige_ejercicio_por_indice(O,N,E,L, pecho,   Dia, E1),
    elige_ejercicio_por_indice(O,N,E,L, hombros, Dia, E2),
    elige_ejercicio_por_indice(O,N,E,L, core,    Dia, E3).

% ----- PULL -----
rutina_pull_dia(O,N,E,L,Dia, [
    [espalda, E1],
    [core,    E2]
]) :-
    elige_ejercicio_por_indice(O,N,E,L, espalda, Dia, E1),
    elige_ejercicio_por_indice(O,N,E,L, core,    Dia, E2).

% ----- LEGS -----
rutina_legs_dia(O,N,E,L,Dia, [
    [piernas, E1],
    [core,    E2]
]) :-
    elige_ejercicio_por_indice(O,N,E,L, piernas, Dia, E1),
    elige_ejercicio_por_indice(O,N,E,L, core,    Dia, E2).

% ----- UPPER -----
rutina_upper_dia(O,N,E,L,Dia, [
    [pecho,   E1],
    [espalda, E2],
    [hombros, E3]
]) :-
    elige_ejercicio_por_indice(O,N,E,L, pecho,   Dia, E1),
    elige_ejercicio_por_indice(O,N,E,L, espalda, Dia, E2),
    elige_ejercicio_por_indice(O,N,E,L, hombros, Dia, E3).

% ----- LOWER -----
rutina_lower_dia(O,N,E,L,Dia, [
    [piernas, E1],
    [core,    E2]
]) :-
    elige_ejercicio_por_indice(O,N,E,L, piernas, Dia, E1),
    elige_ejercicio_por_indice(O,N,E,L, core,    Dia, E2).

% ===========================
% PLANIFICACIÓN
% ===========================

rutina_para_dia(O,N,1,L,E, Dia, R) :- rutina_fullbody_dia(O,N,E,L,Dia,R).
rutina_para_dia(O,N,2,L,E, Dia, R) :- rutina_fullbody_dia(O,N,E,L,Dia,R).

rutina_para_dia(O,N,3,L,E, 1, R) :- rutina_push_dia(O,N,E,L,1,R).
rutina_para_dia(O,N,3,L,E, 2, R) :- rutina_pull_dia(O,N,E,L,2,R).
rutina_para_dia(O,N,3,L,E, 3, R) :- rutina_legs_dia(O,N,E,L,3,R).

rutina_para_dia(O,N,4,L,E, 1, R) :- rutina_upper_dia(O,N,E,L,1,R).
rutina_para_dia(O,N,4,L,E, 2, R) :- rutina_lower_dia(O,N,E,L,2,R).
rutina_para_dia(O,N,4,L,E, 3, R) :- rutina_upper_dia(O,N,E,L,3,R).
rutina_para_dia(O,N,4,L,E, 4, R) :- rutina_lower_dia(O,N,E,L,4,R).

rutina_para_dia(O,N,5,L,E, 1, R) :- rutina_push_dia(O,N,E,L,1,R).
rutina_para_dia(O,N,5,L,E, 2, R) :- rutina_pull_dia(O,N,E,L,2,R).
rutina_para_dia(O,N,5,L,E, 3, R) :- rutina_legs_dia(O,N,E,L,3,R).
rutina_para_dia(O,N,5,L,E, 4, R) :- rutina_upper_dia(O,N,E,L,4,R).
rutina_para_dia(O,N,5,L,E, 5, R) :- rutina_fullbody_dia(O,N,E,L,5,R).

rutina_para_dia(O,N,6,L,E, 1, R) :- rutina_push_dia(O,N,E,L,1,R).
rutina_para_dia(O,N,6,L,E, 2, R) :- rutina_pull_dia(O,N,E,L,2,R).
rutina_para_dia(O,N,6,L,E, 3, R) :- rutina_legs_dia(O,N,E,L,3,R).
rutina_para_dia(O,N,6,L,E, 4, R) :- rutina_push_dia(O,N,E,L,4,R).
rutina_para_dia(O,N,6,L,E, 5, R) :- rutina_pull_dia(O,N,E,L,5,R).
rutina_para_dia(O,N,6,L,E, 6, R) :- rutina_legs_dia(O,N,E,L,6,R).

% ===========================
% GENERACIÓN FINAL
% ===========================

generar_rutina(O,N,Dias,L,E,RutinaSemana) :-
    integer(Dias),
    Dias >= 1,
    Dias =< 6,
    findall(
        [D, RutinaDia],
        (
            between(1, Dias, D),
            rutina_para_dia(O,N,Dias,L,E,D,RutinaDia)
        ),
        RutinaSemana
    ).