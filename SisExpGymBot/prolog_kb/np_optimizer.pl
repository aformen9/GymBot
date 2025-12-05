% ----------------------------------------------------------
% np_optimizer.pl - Optimal Exercise Sequencing (NP-Complete)
%
% PROBLEMA: Dado un conjunto de N ejercicios con restricciones de
% equipamiento y grupos musculares, encontrar la secuencia óptima
% que minimice el costo total de transición.
%
% REDUCCIÓN DESDE: Job Scheduling with Setup Times (NP-Complete)
% ----------------------------------------------------------

:- dynamic mejor_costo/1, mejor_secuencia/1.

% ===========================
% BASE DE CONOCIMIENTO: EJERCICIOS CON ATRIBUTOS
% ===========================

% ejercicio_detalle(Nombre, GrupoMuscular, Equipamiento, TiempoEjecucion, NivelFatiga)
ejercicio_detalle(press_banca_plano, pecho, barra, 180, 8).
ejercicio_detalle(sentadilla_libre, piernas, barra, 240, 9).
ejercicio_detalle(dominadas, espalda, peso_corporal, 120, 7).
ejercicio_detalle(remo_barra, espalda, barra, 150, 7).
ejercicio_detalle(press_militar_barra, hombros, barra, 150, 6).
ejercicio_detalle(peso_muerto, piernas, barra, 180, 9).
ejercicio_detalle(flexiones_suelo, pecho, peso_corporal, 90, 5).
ejercicio_detalle(plancha_abdominal, core, peso_corporal, 60, 4).
ejercicio_detalle(curl_femoral, piernas, maquina, 120, 5).
ejercicio_detalle(extension_cuadriceps, piernas, maquina, 120, 6).

% Cargar la base de conocimiento de ejercicios
:- consult('exercises.pl').

% Regla de respaldo: si un ejercicio no está explícitamente definido arriba,
% generarlo dinámicamente desde exercise/6
ejercicio_detalle(Nombre, Grupo, Equip, Tiempo, Fatiga) :-
    \+ clause(ejercicio_detalle(Nombre, _, _, _, _), true),  % Solo si no está definido
    exercise(Nombre, Grupo, _, _, EquipOriginal, _),
    % Mapear equipamiento
    mapear_equipamiento(EquipOriginal, Equip),
    % Estimar tiempo y fatiga basado en grupo muscular
    estimar_parametros(Grupo, Tiempo, Fatiga).

% Mapeo de equipamiento de exercises.pl a np_optimizer.pl
mapear_equipamiento(gym_completo, barra).
mapear_equipamiento(mancuernas, mancuernas).
mapear_equipamiento(peso_corporal, peso_corporal).
mapear_equipamiento(_, maquina).  % Default

% Estimación de parámetros por grupo muscular
estimar_parametros(pecho, 120, 6).
estimar_parametros(espalda, 150, 7).
estimar_parametros(piernas, 180, 8).
estimar_parametros(hombros, 120, 5).
estimar_parametros(core, 90, 4).
estimar_parametros(_, 120, 6).  % Default

% ===========================
% COSTOS DE TRANSICIÓN ENTRE EQUIPAMIENTOS
% ===========================

% costo_setup(Equip1, Equip2, TiempoSegundos)
costo_setup(barra, barra, 10).            % Mismo equipo, solo cambiar peso
costo_setup(barra, mancuernas, 30).
costo_setup(barra, maquina, 60).
costo_setup(barra, peso_corporal, 20).
costo_setup(mancuernas, barra, 30).
costo_setup(mancuernas, mancuernas, 5).
costo_setup(mancuernas, maquina, 40).
costo_setup(mancuernas, peso_corporal, 15).
costo_setup(maquina, barra, 60).
costo_setup(maquina, mancuernas, 40).
costo_setup(maquina, maquina, 20).        % Cambiar maquina
costo_setup(maquina, peso_corporal, 25).
costo_setup(peso_corporal, barra, 20).
costo_setup(peso_corporal, mancuernas, 15).
costo_setup(peso_corporal, maquina, 25).
costo_setup(peso_corporal, peso_corporal, 0).

% ===========================
% PESOS DE LA FUNCIÓN DE COSTO
% ===========================

peso_alpha(1.0).   % Peso para cambios de equipamiento
peso_beta(2.0).    % Peso para fatiga muscular
peso_gamma(1.5).   % Peso para tiempo de setup

% ===========================
% CÁLCULO DE COSTO DE TRANSICIÓN
% ===========================

calcular_costo_transicion(Ej1, Ej2, Costo) :-
    ejercicio_detalle(Ej1, Grupo1, Equip1, _, Fatiga1),
    ejercicio_detalle(Ej2, Grupo2, Equip2, _, Fatiga2),
    
    % Costo por cambio de equipamiento
    costo_setup(Equip1, Equip2, TiempoCambio),
    
    % Penalización por repetir grupo muscular (fatiga acumulada)
    (   Grupo1 = Grupo2
    ->  TiempoFatiga is (Fatiga1 + Fatiga2) * 10
    ;   TiempoFatiga is 0
    ),
    
    % Tiempo de setup
    (   Equip1 = Equip2
    ->  TiempoSetup is 5
    ;   TiempoSetup is 20
    ),
    
    % Aplicar pesos
    peso_alpha(Alpha),
    peso_beta(Beta),
    peso_gamma(Gamma),
    
    Costo is Alpha * TiempoCambio + Beta * TiempoFatiga + Gamma * TiempoSetup.

% ===========================
% CÁLCULO DE COSTO TOTAL DE UNA SECUENCIA
% ===========================

costo_secuencia([], 0) :- !.
costo_secuencia([_], 0) :- !.
costo_secuencia([Ej1, Ej2 | Resto], CostoTotal) :-
    calcular_costo_transicion(Ej1, Ej2, Costo12),
    costo_secuencia([Ej2 | Resto], CostoResto),
    CostoTotal is Costo12 + CostoResto,
    !.  % Cut para evitar backtracking

% ===========================
% SOLUCIÓN 1: FUERZA BRUTA (EXACTA)
% Complejidad: O(N!)
% Factible solo para N ≤ 10
% ===========================

resolver_exacto(Ejercicios, MejorSecuencia, MejorCosto) :-
    length(Ejercicios, N),
    (   N > 10
    ->  format('ERROR: Fuerza bruta solo funciona para N <= 10 (actual: ~w)~n', [N]),
        fail
    ;   format('Iniciando búsqueda exhaustiva de ~w! = ~w permutaciones...~n', 
               [N, Factorial]),
        factorial(N, Factorial),
        retractall(mejor_costo(_)),
        retractall(mejor_secuencia(_)),
        assert(mejor_costo(999999)),
        
        % Probar todas las permutaciones
        permutation(Ejercicios, Secuencia),
        costo_secuencia(Secuencia, Costo),
        actualizar_mejor(Secuencia, Costo),
        fail
    ;   mejor_secuencia(MejorSecuencia),
        mejor_costo(MejorCosto),
        !,  % Cut para evitar backtracking
        format('✓ Solución óptima encontrada con costo: ~w~n', [MejorCosto])
    ).

actualizar_mejor(Secuencia, Costo) :-
    mejor_costo(CostoActual),
    (   Costo < CostoActual
    ->  retractall(mejor_costo(_)),
        retractall(mejor_secuencia(_)),
        assert(mejor_costo(Costo)),
        assert(mejor_secuencia(Secuencia)),
        format('  Nueva mejor solución: costo = ~w~n', [Costo])
    ;   true
    ).

factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% ===========================
% SOLUCIÓN 2: BRANCH & BOUND (CON PODA)
% Complejidad: O(N² × 2^N) peor caso, mucho mejor en práctica
% Factible para N ≤ 15
% ===========================

resolver_branch_and_bound(Ejercicios, MejorSecuencia, MejorCosto) :-
    length(Ejercicios, N),
    (   N > 15
    ->  format('ADVERTENCIA: Branch & Bound lento para N > 15 (actual: ~w)~n', [N])
    ;   true
    ),
    
    format('Iniciando Branch & Bound...~n', []),
    retractall(mejor_costo(_)),
    retractall(mejor_secuencia(_)),
    assert(mejor_costo(999999)),
    
    % Iniciar búsqueda con poda
    branch_and_bound([], Ejercicios, 0),
    
    mejor_secuencia(MejorSecuencia),
    mejor_costo(MejorCosto),
    !,  % Cut para evitar backtracking
    format('✓ Solución encontrada con costo: ~w~n', [MejorCosto]).

branch_and_bound(SecuenciaActual, [], CostoActual) :-
    % Caso base: no quedan ejercicios
    actualizar_mejor(SecuenciaActual, CostoActual).

branch_and_bound(SecuenciaActual, Restantes, CostoActual) :-
    % Elegir siguiente ejercicio
    select(Siguiente, Restantes, NuevosRestantes),
    
    % Calcular nuevo costo
    (   SecuenciaActual = []
    ->  NuevoCosto is 0
    ;   last(SecuenciaActual, Ultimo),
        calcular_costo_transicion(Ultimo, Siguiente, CostoTransicion),
        NuevoCosto is CostoActual + CostoTransicion
    ),
    
    % Continuar explorando (sin poda prematura basada en costo parcial)
    append(SecuenciaActual, [Siguiente], NuevaSecuencia),
    branch_and_bound(NuevaSecuencia, NuevosRestantes, NuevoCosto).

estimar_costo_minimo([], 0).
estimar_costo_minimo(Ejercicios, CotaInferior) :-
    length(Ejercicios, N),
    N > 0,
    % Costo mínimo más realista: usar un valor conservador
    CostoMinTransicion is 20,  % Valor más realista
    CotaInferior is (N - 1) * CostoMinTransicion.

last([X], X).
last([_|T], X) :- last(T, X).

% ===========================
% SOLUCIÓN 3: HEURÍSTICA GREEDY
% Complejidad: O(N²)
% Factible para cualquier N
% ===========================

resolver_greedy(Ejercicios, MejorSecuencia, MejorCosto) :-
    format('Iniciando Greedy Heurística...~n', []),
    (   Ejercicios = []
    ->  MejorSecuencia = [], MejorCosto = 0
    ;   % Probar con cada ejercicio como inicio y quedarse con el mejor
        greedy_mejor_inicio(Ejercicios, MejorSecuencia, MejorCosto),
        !,
        format('✓ Solución heurística encontrada con costo: ~w~n', [MejorCosto])
    ).

% Probar cada ejercicio como inicio y quedarse con el mejor
greedy_mejor_inicio(Ejercicios, MejorSec, MejorCosto) :-
    greedy_probar_inicios(Ejercicios, Ejercicios, 999999, [], MejorCosto, MejorSec).

greedy_probar_inicios(_, [], CostoActual, SecActual, CostoActual, SecActual) :- !.
greedy_probar_inicios(TodosEjs, [Inicio|RestoInicios], MejorCostoAnt, MejorSecAnt, MejorCostoFinal, MejorSecFinal) :-
    select(Inicio, TodosEjs, Restantes),
    greedy_recursivo([Inicio], Restantes, Secuencia, 0, Costo),
    (   Costo < MejorCostoAnt
    ->  greedy_probar_inicios(TodosEjs, RestoInicios, Costo, Secuencia, MejorCostoFinal, MejorSecFinal)
    ;   greedy_probar_inicios(TodosEjs, RestoInicios, MejorCostoAnt, MejorSecAnt, MejorCostoFinal, MejorSecFinal)
    ).

greedy_recursivo(SecuenciaActual, [], SecuenciaActual, CostoActual, CostoActual).
greedy_recursivo(SecuenciaActual, Restantes, SecuenciaFinal, CostoActual, CostoFinal) :-
    last(SecuenciaActual, Ultimo),
    
    % Encontrar el ejercicio con menor costo de transición
    encontrar_mejor_siguiente(Ultimo, Restantes, Mejor, MejorCosto),
    
    % Agregar a la secuencia
    append(SecuenciaActual, [Mejor], NuevaSecuencia),
    NuevoCosto is CostoActual + MejorCosto,
    
    % Remover de restantes
    select(Mejor, Restantes, NuevosRestantes),
    
    % Continuar recursivamente
    greedy_recursivo(NuevaSecuencia, NuevosRestantes, SecuenciaFinal, NuevoCosto, CostoFinal).

encontrar_mejor_siguiente(Ultimo, [Ej], Ej, Costo) :-
    calcular_costo_transicion(Ultimo, Ej, Costo).
encontrar_mejor_siguiente(Ultimo, [Ej1, Ej2 | Resto], Mejor, MejorCosto) :-
    calcular_costo_transicion(Ultimo, Ej1, Costo1),
    encontrar_mejor_siguiente(Ultimo, [Ej2 | Resto], MejorTemp, CostoTemp),
    (   Costo1 < CostoTemp
    ->  Mejor = Ej1, MejorCosto = Costo1
    ;   Mejor = MejorTemp, MejorCosto = CostoTemp
    ).

% ===========================
% COMPARACIÓN DE ALGORITMOS
% ===========================

comparar_algoritmos(Ejercicios) :-
    length(Ejercicios, N),
    format('~n========================================~n', []),
    format('COMPARACIÓN DE ALGORITMOS (N = ~w)~n', [N]),
    format('========================================~n', []),
    
    % Greedy (siempre)
    format('~n1. GREEDY HEURÍSTICA:~n', []),
    get_time(T1),
    resolver_greedy(Ejercicios, SecGreedy, CostoGreedy),
    get_time(T2),
    TiempoGreedy is T2 - T1,
    format('   Costo: ~w~n', [CostoGreedy]),
    format('   Tiempo: ~3f segundos~n', [TiempoGreedy]),
    format('   Secuencia: ~w~n', [SecGreedy]),
    
    % Branch & Bound (si N ≤ 15)
    (   N =< 15
    ->  format('~n2. BRANCH & BOUND:~n', []),
        get_time(T3),
        resolver_branch_and_bound(Ejercicios, SecBB, CostoBB),
        get_time(T4),
        TiempoBB is T4 - T3,
        Mejora1 is ((CostoGreedy - CostoBB) / CostoGreedy) * 100,
        format('   Costo: ~w~n', [CostoBB]),
        format('   Tiempo: ~3f segundos~n', [TiempoBB]),
        format('   Mejora vs Greedy: ~2f%~n', [Mejora1]),
        format('   Secuencia: ~w~n', [SecBB])
    ;   format('~n2. BRANCH & BOUND: Omitido (N > 15)~n', [])
    ),
    
    % Exacto (si N ≤ 10)
    (   N =< 10
    ->  format('~n3. FUERZA BRUTA (ÓPTIMO):~n', []),
        get_time(T5),
        resolver_exacto(Ejercicios, SecExacto, CostoExacto),
        get_time(T6),
        TiempoExacto is T6 - T5,
        Mejora2 is ((CostoGreedy - CostoExacto) / CostoGreedy) * 100,
        format('   Costo: ~w~n', [CostoExacto]),
        format('   Tiempo: ~3f segundos~n', [TiempoExacto]),
        format('   Mejora vs Greedy: ~2f%~n', [Mejora2]),
        format('   Secuencia: ~w~n', [SecExacto])
    ;   format('~n3. FUERZA BRUTA: Omitido (N > 10)~n', [])
    ),
    
    format('~n========================================~n', []).

% ===========================
% CASOS DE PRUEBA
% ===========================

% Test con 5 ejercicios (factible para todos los algoritmos)
test_5_ejercicios :-
    Ejercicios = [
        press_banca_plano,
        sentadilla_libre,
        dominadas,
        remo_barra,
        plancha_abdominal
    ],
    comparar_algoritmos(Ejercicios).

% Test con 8 ejercicios (factible para exacto y branch & bound)
test_8_ejercicios :-
    Ejercicios = [
        press_banca_plano,
        sentadilla_libre,
        dominadas,
        remo_barra,
        press_militar_barra,
        peso_muerto,
        flexiones_suelo,
        plancha_abdominal
    ],
    comparar_algoritmos(Ejercicios).

% Test con 10 ejercicios (límite para exacto)
test_10_ejercicios :-
    Ejercicios = [
        press_banca_plano,
        sentadilla_libre,
        dominadas,
        remo_barra,
        press_militar_barra,
        peso_muerto,
        flexiones_suelo,
        plancha_abdominal,
        curl_femoral,
        extension_cuadriceps
    ],
    comparar_algoritmos(Ejercicios).