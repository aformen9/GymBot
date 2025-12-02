% ----------------------------------------------------------
% rules.pl  - Motor experto de selección de rutinas (completo)
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

% gym_completo sirve para todo
equip_compatible(gym_completo, _).
equip_compatible(E, E).

% ===========================
% LESIONES
% ===========================

compatible_lesion(LesionUsuario, LesionContra) :-
    LesionContra == ninguna ;
    LesionUsuario \= LesionContra.

% ===========================
% SELECCIÓN DE EJERCICIOS
% ===========================

ejercicio_apto(Objetivo, Nivel, Equip, Lesion, Grupo, Nombre) :-
    exercise(Nombre, Grupo, Objetivo, NivelMin, EquipReq, LesionContra),
    nivel_suficiente(Nivel, NivelMin),
    equip_compatible(Equip, EquipReq),
    compatible_lesion(Lesion, LesionContra).

elige_ejercicio(O,N,E,L, Grupo, Nombre) :-
    ejercicio_apto(O,N,E,L, Grupo, Nombre),
    !.

% ===========================
% VARIACIÓN → lista de ejercicios y selección por índice/día
% ===========================

lista_ejercicios_aptos(O,N,E,L,Grupo,Lista) :-
    findall(
        Nombre,
        ejercicio_apto(O,N,E,L,Grupo,Nombre),
        Lista
    ).

elige_ejercicio_por_indice(O,N,E,L,Grupo,Dia,Nombre) :-
    lista_ejercicios_aptos(O,N,E,L,Grupo,Lista),
    Lista \= [],
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
% PLANIFICACIÓN SEGÚN CANTIDAD DE DÍAS
% ===========================

% 1 día → Fullbody
rutina_para_dia(O,N,1,L,E, Dia, R) :-
    rutina_fullbody_dia(O,N,E,L,Dia,R).

% 2 días → Fullbody + Fullbody variado
rutina_para_dia(O,N,2,L,E, Dia, R) :-
    rutina_fullbody_dia(O,N,E,L,Dia,R).

% 3 días → Push / Pull / Legs
rutina_para_dia(O,N,3,L,E, 1, R) :- rutina_push_dia(O,N,E,L,1,R).
rutina_para_dia(O,N,3,L,E, 2, R) :- rutina_pull_dia(O,N,E,L,2,R).
rutina_para_dia(O,N,3,L,E, 3, R) :- rutina_legs_dia(O,N,E,L,3,R).

% 4 días → Upper / Lower / Upper / Lower
rutina_para_dia(O,N,4,L,E, 1, R) :- rutina_upper_dia(O,N,E,L,1,R).
rutina_para_dia(O,N,4,L,E, 2, R) :- rutina_lower_dia(O,N,E,L,2,R).
rutina_para_dia(O,N,4,L,E, 3, R) :- rutina_upper_dia(O,N,E,L,3,R).
rutina_para_dia(O,N,4,L,E, 4, R) :- rutina_lower_dia(O,N,E,L,4,R).

% 5 días → Push / Pull / Legs / Upper / Fullbody
rutina_para_dia(O,N,5,L,E, 1, R) :- rutina_push_dia(O,N,E,L,1,R).
rutina_para_dia(O,N,5,L,E, 2, R) :- rutina_pull_dia(O,N,E,L,2,R).
rutina_para_dia(O,N,5,L,E, 3, R) :- rutina_legs_dia(O,N,E,L,3,R).
rutina_para_dia(O,N,5,L,E, 4, R) :- rutina_upper_dia(O,N,E,L,4,R).
rutina_para_dia(O,N,5,L,E, 5, R) :- rutina_fullbody_dia(O,N,E,L,5,R).

% 6 días → Push / Pull / Legs / Push / Pull / Legs
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
