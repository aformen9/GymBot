"""
routine_builder.py - Constructor avanzado de rutinas
Lógica adicional para personalización y optimización de rutinas
"""

from typing import List, Dict, Tuple
import random


class RoutineBuilder:
    """
    Clase para construir y personalizar rutinas de entrenamiento.
    """

    def __init__(self):
        self.ejercicios_compuestos = {
            "pecho": [
                "press_banca_plano",
                "press_banca_inclinado",
                "press_banca_barra",
            ],
            "espalda": ["remo_barra", "peso_muerto", "dominadas", "jalon_pecho"],
            "piernas": [
                "sentadilla_libre",
                "peso_muerto",
                "prensa_45",
                "sentadilla_frontal",
            ],
            "hombros": ["press_militar_barra", "press_militar_pesado"],
            "core": ["rollout_rueda", "dragon_flag"],
        }

        self.ejercicios_aislamiento = {
            "pecho": [
                "aperturas_mancuernas",
                "contractora_pecho",
                "flexiones_diamante",
            ],
            "espalda": ["remo_mancuerna", "pullover_mancuernas", "face_pull"],
            "piernas": ["extension_cuadriceps", "curl_femoral", "elevacion_talones"],
            "hombros": [
                "elevaciones_laterales",
                "elevaciones_frontales",
                "pajaros_mancuernas",
            ],
            "core": ["crunch_abdominal", "plancha_abdominal", "russian_twist"],
        }

    def balancear_rutina(self, rutina: List, objetivo: str) -> List:
        """
        Balancea la rutina para asegurar proporciones adecuadas entre grupos.
        """
        volumen_grupos = self._calcular_volumen(rutina)

        # Definir proporciones ideales según objetivo
        proporciones_ideales = self._obtener_proporciones_ideales(objetivo)

        # Ajustar si hay desbalances significativos
        rutina_balanceada = self._ajustar_volumen(
            rutina, volumen_grupos, proporciones_ideales
        )

        return rutina_balanceada

    def _calcular_volumen(self, rutina: List) -> Dict[str, int]:
        """Calcula sets totales por grupo muscular."""
        volumen = {"pecho": 0, "espalda": 0, "piernas": 0, "hombros": 0, "core": 0}

        for dia_info in rutina:
            ejercicios = dia_info[1]
            for grupo, _ in ejercicios:
                if grupo in volumen:
                    volumen[grupo] += 1

        return volumen

    def _obtener_proporciones_ideales(self, objetivo: str) -> Dict[str, float]:
        """
        Retorna proporciones ideales de volumen según objetivo.
        """
        if objetivo == "ganar_masa":
            return {
                "pecho": 0.25,
                "espalda": 0.25,
                "piernas": 0.30,
                "hombros": 0.12,
                "core": 0.08,
            }
        elif objetivo == "fuerza":
            return {
                "pecho": 0.22,
                "espalda": 0.28,
                "piernas": 0.35,
                "hombros": 0.10,
                "core": 0.05,
            }
        else:  # bajar_grasa
            return {
                "pecho": 0.20,
                "espalda": 0.20,
                "piernas": 0.30,
                "hombros": 0.15,
                "core": 0.15,
            }

    def _ajustar_volumen(
        self, rutina: List, volumen_actual: Dict, proporciones: Dict
    ) -> List:
        """
        Ajusta el volumen de la rutina según proporciones ideales.
        """
        # Por ahora retorna la rutina sin cambios
        # En una implementación completa, aquí se rebalancearía
        _ = volumen_actual  # Evitar warning de variable no usada
        _ = proporciones  # Evitar warning de variable no usada
        return rutina

    def agregar_calentamiento(self, rutina: List, nivel: str) -> List:
        """
        Agrega ejercicios de calentamiento específicos por día.
        """
        calentamientos = {
            "principiante": ["Movilidad articular 5min", "Cardio ligero 5min"],
            "intermedio": [
                "Movilidad articular 5min",
                "Cardio ligero 5min",
                "Series de activación",
            ],
            "avanzado": [
                "Movilidad dinámica 7min",
                "Cardio moderado 5min",
                "Series de activación",
                "Sets de aproximación",
            ],
        }

        rutina_con_calentamiento = []

        for dia_info in rutina:
            dia = dia_info[0]
            ejercicios = dia_info[1]

            # Agregar calentamiento al inicio
            calentamiento_dia = [
                ("calentamiento", cal)
                for cal in calentamientos.get(nivel, calentamientos["principiante"])
            ]

            ejercicios_completos = calentamiento_dia + ejercicios

            if len(dia_info) > 2:
                rutina_con_calentamiento.append(
                    [dia, ejercicios_completos, dia_info[2], dia_info[3]]
                )
            else:
                rutina_con_calentamiento.append([dia, ejercicios_completos])

        return rutina_con_calentamiento

    def sugerir_superseries(self, rutina: List, nivel: str) -> List[Tuple[str, str]]:
        """
        Sugiere pares de ejercicios para hacer en superserie.
        """
        if nivel == "principiante":
            return []

        superseries = []

        for dia_info in rutina:
            ejercicios = dia_info[1]
            grupos = [grupo for grupo, _ in ejercicios]

            # Buscar grupos antagonistas
            if "pecho" in grupos and "espalda" in grupos:
                idx_pecho = grupos.index("pecho")
                idx_espalda = grupos.index("espalda")
                superseries.append(
                    (ejercicios[idx_pecho][1], ejercicios[idx_espalda][1])
                )

        return superseries

    def optimizar_orden_ejercicios(self, rutina: List, objetivo: str) -> List:
        """
        Reordena ejercicios siguiendo principios de entrenamiento óptimos.
        Regla general: Compuestos primero, aislamiento después.
        """
        _ = objetivo  # Variable usada para futura lógica de optimización

        rutina_optimizada = []

        for dia_info in rutina:
            dia = dia_info[0]
            ejercicios = dia_info[1]

            # Separar en compuestos y aislamiento
            compuestos = []
            aislamiento = []
            otros = []

            for grupo, ejercicio in ejercicios:
                if self._es_compuesto(grupo, ejercicio):
                    compuestos.append((grupo, ejercicio))
                elif self._es_aislamiento(grupo, ejercicio):
                    aislamiento.append((grupo, ejercicio))
                else:
                    otros.append((grupo, ejercicio))

            # Ordenar: compuestos -> aislamiento -> otros (calentamiento/core)
            ejercicios_ordenados = compuestos + aislamiento + otros

            if len(dia_info) > 2:
                rutina_optimizada.append(
                    [dia, ejercicios_ordenados, dia_info[2], dia_info[3]]
                )
            else:
                rutina_optimizada.append([dia, ejercicios_ordenados])

        return rutina_optimizada

    def _es_compuesto(self, grupo: str, ejercicio: str) -> bool:
        """Verifica si un ejercicio es compuesto."""
        return ejercicio in self.ejercicios_compuestos.get(grupo, [])

    def _es_aislamiento(self, grupo: str, ejercicio: str) -> bool:
        """Verifica si un ejercicio es de aislamiento."""
        return ejercicio in self.ejercicios_aislamiento.get(grupo, [])

    def generar_variacion(self, rutina: List, porcentaje: float = 0.3) -> List:
        """
        Genera una variación de la rutina cambiando un porcentaje de ejercicios.
        Útil para evitar estancamiento.
        """
        rutina_variada = []

        for dia_info in rutina:
            dia = dia_info[0]
            ejercicios = dia_info[1]

            ejercicios_variados = ejercicios.copy()
            num_cambios = max(1, int(len(ejercicios) * porcentaje))

            # Seleccionar ejercicios al azar para cambiar
            _ = random.sample(range(len(ejercicios)), num_cambios)

            # Por ahora solo marca los que se cambiarían
            # En implementación completa, buscaría alternativas en la base de datos

            if len(dia_info) > 2:
                rutina_variada.append(
                    [dia, ejercicios_variados, dia_info[2], dia_info[3]]
                )
            else:
                rutina_variada.append([dia, ejercicios_variados])

        return rutina_variada

    def calcular_tiempo_estimado(self, rutina: List, nivel: str) -> Dict[int, int]:
        """
        Calcula tiempo estimado de entrenamiento por día en minutos.
        """
        tiempos = {}

        # Tiempos base por ejercicio según nivel (en minutos)
        tiempo_por_ejercicio = {
            "principiante": 8,  # Más descanso, más explicaciones
            "intermedio": 6,
            "avanzado": 5,
        }

        tiempo_base = tiempo_por_ejercicio.get(nivel, 6)
        tiempo_calentamiento = 10
        tiempo_estiramiento = 5

        for dia_info in rutina:
            dia = dia_info[0]
            num_ejercicios = len(dia_info[1])

            tiempo_total = (
                tiempo_calentamiento
                + (num_ejercicios * tiempo_base)
                + tiempo_estiramiento
            )

            tiempos[dia] = tiempo_total

        return tiempos


# Funciones auxiliares standalone
def crear_plan_deload(rutina_normal: List) -> List:
    """
    Crea una semana de deload (descarga) reduciendo volumen e intensidad.
    """
    rutina_deload = []

    for dia_info in rutina_normal:
        dia = dia_info[0]
        ejercicios = dia_info[1]

        # Reducir número de ejercicios (60% del original)
        num_ejercicios_deload = max(2, int(len(ejercicios) * 0.6))
        ejercicios_deload = ejercicios[:num_ejercicios_deload]

        if len(dia_info) > 2:
            # Reducir series (50-60% del volumen normal)
            series_reducidas = max(2, int(dia_info[2] * 0.6))
            rutina_deload.append(
                [dia, ejercicios_deload, series_reducidas, dia_info[3]]
            )
        else:
            rutina_deload.append([dia, ejercicios_deload])

    return rutina_deload


def generar_notas_tecnica(ejercicio: str) -> str:
    """
    Retorna notas técnicas breves para ejercicios comunes.
    """
    notas = {
        "press_banca_plano": "Mantén escápulas retraídas, pies firmes en el suelo. Baja la barra al pecho con control.",
        "sentadilla_libre": "Rodillas alineadas con pies, espalda neutra. Desciende hasta paralelo o más.",
        "peso_muerto": "Espalda recta, caderas y rodillas extendidas simultáneamente. Barra cerca del cuerpo.",
        "dominadas": "Agarre pronado, descenso controlado, barbilla sobre la barra.",
        "press_militar": "Core activado, evitar hiperextensión lumbar. Barra en línea vertical.",
    }

    return notas.get(ejercicio, "Mantén buena técnica y control en todo el movimiento.")
