# Problema NP-Completo: Optimal Exercise Sequencing

## Descripción

**Problema:** Dado un conjunto de N ejercicios con restricciones de equipamiento y grupos musculares, encontrar la secuencia óptima que minimice el costo total.

**Función de costo:**
```
C(S) = α·T_equipamiento + β·T_fatiga + γ·T_setup
```

## Demostración de NP-Completitud

**Reducción desde:** Job Scheduling with Setup Times (JSST) - NP-Completo probado

**Mapeo:**
- Trabajo → Ejercicio
- Tiempo de procesamiento → Tiempo de ejecución
- Setup time → Costo de transición de equipamiento
- Restricción de máquina → Equipamiento requerido

**Conclusión:** Si OESS ∈ P, entonces JSST ∈ P. Como JSST es NP-Completo, OESS también lo es.

## Algoritmos Implementados

### 1. Fuerza Bruta (Exacto)
- **Complejidad:** O(N!)
- **Factible:** N ≤ 10
- **Garantía:** Solución óptima

### 2. Branch & Bound
- **Complejidad:** O(N² × 2^N) peor caso
- **Factible:** N ≤ 15
- **Mejora:** Poda de ramas sub-óptimas

### 3. Greedy Heurística
- **Complejidad:** O(N²)
- **Factible:** Cualquier N
- **Aproximación:** Sin garantía de óptimo

## Uso
```prolog
% Cargar el archivo
?- [np_optimizer].

% Test con 5 ejercicios
?- test_5_ejercicios.

% Test con 8 ejercicios
?- test_8_ejercicios.

% Test con 10 ejercicios (límite para exacto)
?- test_10_ejercicios.

% Comparar algoritmos con lista personalizada
?- comparar_algoritmos([press_banca_plano, sentadilla_libre, dominadas]).
```

## Resultados Esperados

Para N=5:
- Fuerza Bruta: ~0.01s
- Branch & Bound: ~0.005s
- Greedy: ~0.001s

Para N=10:
- Fuerza Bruta: ~30s
- Branch & Bound: ~2s
- Greedy: ~0.002s