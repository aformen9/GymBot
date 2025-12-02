% ----------------------------------------------------------
% rules.pl  - Motor experto con sistema de lesiones MEJORADO
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

equip_compatible(gym_completo, _).
equip_compatible(mancuernas, peso_corporal). 
equip_compatible(E, E).

% ===========================
% LESIONES - Sistema mejorado por categorías
% ===========================

% Sin lesiones: todo compatible
compatible_lesion(ninguna, _).

% Con lesión: evitar ejercicios contraindicados para esa categoría
compatible_lesion(LesionUsuario, LesionContra) :-
    LesionUsuario \= ninguna,
    (LesionContra == ninguna ; LesionUsuario \= LesionContra).

% ===========================
% SISTEMA DE VOLUMEN Y PROGRESIÓN
% ===========================

volumen_recomendado(principiante, ganar_masa, 3, 12).
volumen_recomendado(principiante, bajar_grasa, 3, 15).
volumen_recomendado(principiante, fuerza, 4, 6).

volumen_recomendado(intermedio, ganar_masa, 4, 10).
volumen_recomendado(intermedio, bajar_grasa, 3, 15).
volumen_recomendado(intermedio, fuerza, 5, 5).

volumen_recomendado(avanzado, ganar_masa, 5, 8).
volumen_recomendado(avanzado, bajar_grasa, 4, 12).
volumen_recomendado(avanzado, fuerza, 5, 3).

% ===========================
% SELECCIÓN DE EJERCICIOS
% ===========================

ejercicio_apto(Objetivo, Nivel, Equip, Lesion, Grupo, Nombre) :-
    exercise(Nombre, Grupo, Objetivo, NivelMin, EquipReq, LesionContra),
    nivel_suficiente(Nivel, NivelMin),
    equip_compatible(Equip, EquipReq),
    compatible_lesion(Lesion, LesionContra).

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
    list_to_set(ListaBruta, Lista).

elige_ejercicio_por_indice(O,N,E,L,Grupo,Dia,Nombre) :-
    lista_ejercicios_aptos(O,N,E,L,Grupo,Lista),
    Lista \= [],
    length(Lista, Len),
    I0 is (Dia - 1) mod Len,
    nth0(I0, Lista, Nombre).

% ===========================
% RUTINAS CON VARIACIÓN POR DÍA
% ===========================

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

rutina_push_dia(O,N,E,L,Dia, [
    [pecho,   E1],
    [pecho,   E2],
    [hombros, E3],
    [core,    E4]
]) :-
    elige_ejercicio_por_indice(O,N,E,L, pecho,   Dia, E1),
    elige_ejercicio_por_indice(O,N,E,L, pecho,   (Dia+1), E2),
    elige_ejercicio_por_indice(O,N,E,L, hombros, Dia, E3),
    elige_ejercicio_por_indice(O,N,E,L, core,    Dia, E4).

rutina_pull_dia(O,N,E,L,Dia, [
    [espalda, E1],
    [espalda, E2],
    [hombros, E3],
    [core,    E4]
]) :-
    elige_ejercicio_por_indice(O,N,E,L, espalda, Dia, E1),
    elige_ejercicio_por_indice(O,N,E,L, espalda, (Dia+1), E2),
    elige_ejercicio_por_indice(O,N,E,L, hombros, Dia, E3),
    elige_ejercicio_por_indice(O,N,E,L, core,    Dia, E4).

rutina_legs_dia(O,N,E,L,Dia, [
    [piernas, E1],
    [piernas, E2],
    [piernas, E3],
    [core,    E4]
]) :-
    elige_ejercicio_por_indice(O,N,E,L, piernas, Dia, E1),
    elige_ejercicio_por_indice(O,N,E,L, piernas, (Dia+1), E2),
    elige_ejercicio_por_indice(O,N,E,L, piernas, (Dia+2), E3),
    elige_ejercicio_por_indice(O,N,E,L, core,    Dia, E4).

rutina_upper_dia(O,N,E,L,Dia, [
    [pecho,   E1],
    [espalda, E2],
    [hombros, E3],
    [core,    E4]
]) :-
    elige_ejercicio_por_indice(O,N,E,L, pecho,   Dia, E1),
    elige_ejercicio_por_indice(O,N,E,L, espalda, Dia, E2),
    elige_ejercicio_por_indice(O,N,E,L, hombros, Dia, E3),
    elige_ejercicio_por_indice(O,N,E,L, core,    Dia, E4).

rutina_lower_dia(O,N,E,L,Dia, [
    [piernas, E1],
    [piernas, E2],
    [core,    E3]
]) :-
    elige_ejercicio_por_indice(O,N,E,L, piernas, Dia, E1),
    elige_ejercicio_por_indice(O,N,E,L, piernas, (Dia+1), E2),
    elige_ejercicio_por_indice(O,N,E,L, core,    Dia, E3).

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
    volumen_recomendado(N, O, Series, Reps),
    findall(
        [D, RutinaDia, Series, Reps],
        (
            between(1, Dias, D),
            rutina_para_dia(O,N,Dias,L,E,D,RutinaDia)
        ),
        RutinaSemana
    ).