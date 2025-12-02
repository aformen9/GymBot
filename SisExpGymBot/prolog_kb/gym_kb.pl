% ----------------------------------------------------------
% gym_kb.pl  - Archivo principal de la base de conocimiento
% ----------------------------------------------------------

:- [exercises].   % carga exercises.pl
:- [rules].       % carga rules.pl

% Wrapper principal: Prolog expone este predicado a Python.

% plan_rutina(+Objetivo, +Nivel, +Dias, +Lesion, +Equipamiento, -RutinaSemana)
% Ejemplo de llamada:
%   plan_rutina(ganar_masa, principiante, 3, ninguna, gym_completo, Rutina).

plan_rutina(Objetivo, Nivel, Dias, Lesion, Equipamiento, RutinaSemana) :-
    generar_rutina(Objetivo, Nivel, Dias, Lesion, Equipamiento, RutinaSemana).

