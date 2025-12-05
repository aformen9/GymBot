"""
utils.py - Utilidades generales para GymBot
Funciones de formateo, validación y conversión de datos
"""

import json
from datetime import datetime
from typing import Dict, List, Tuple, Any


def formatear_nombre_ejercicio(nombre: str) -> str:
    """
    Convierte nombres de ejercicios de snake_case a formato legible.
    Ejemplo: 'press_banca_plano' -> 'Press Banca Plano'
    """
    return nombre.replace("_", " ").title()


def formatear_grupo_muscular(grupo: str) -> str:
    """
    Convierte nombres de grupos musculares a formato con emoji.
    """
    emojis = {
        "pecho": "💪 Pecho",
        "espalda": "🔙 Espalda",
        "piernas": "🦵 Piernas",
        "hombros": "🤷 Hombros",
        "core": "🎯 Core",
    }
    return emojis.get(grupo.lower(), grupo.title())


def validar_parametros_rutina(
    objetivo: str, nivel: str, dias: int, lesion: str, equipamiento: str
) -> Tuple[bool, str]:
    """
    Valida que los parámetros de la rutina sean correctos.
    Retorna: (es_valido, mensaje_error)
    """
    # Si algún campo está vacío, no validar (es GET inicial)
    if not objetivo or not nivel or not equipamiento:
        return True, ""

    objetivos_validos = ["ganar_masa", "bajar_grasa", "fuerza"]
    niveles_validos = ["principiante", "intermedio", "avanzado"]
    lesiones_validas = [
        "ninguna",
        "extremidad_superior",
        "extremidad_inferior",
        "espalda_lumbar",
        "espalda_cervical",
        "core",
    ]
    equipos_validos = ["peso_corporal", "mancuernas", "gym_completo"]

    if objetivo not in objetivos_validos:
        return False, f"Objetivo inválido. Debe ser uno de: {objetivos_validos}"

    if nivel not in niveles_validos:
        return False, f"Nivel inválido. Debe ser uno de: {niveles_validos}"

    if not isinstance(dias, int) or dias < 1 or dias > 6:
        return False, "Los días deben ser un número entre 1 y 6"

    if lesion not in lesiones_validas:
        return False, f"Lesión inválida. Debe ser una de: {lesiones_validas}"

    if equipamiento not in equipos_validos:
        return False, f"Equipamiento inválido. Debe ser uno de: {equipos_validos}"

    return True, ""


def rutina_a_texto(
    rutina: List, objetivo: str, nivel: str, dias: int, lesion: str, equip: str
) -> str:
    """
    Convierte una rutina a formato texto para descarga.
    """
    texto = "=" * 60 + "\n"
    texto += "             PLAN DE ENTRENAMIENTO GYMBOT\n"
    texto += "=" * 60 + "\n\n"

    texto += "📋 CONFIGURACIÓN\n"
    texto += f"   • Objetivo: {formatear_nombre_ejercicio(objetivo)}\n"
    texto += f"   • Nivel: {nivel.title()}\n"
    texto += f"   • Frecuencia: {dias} días/semana\n"
    texto += f"   • Equipamiento: {formatear_nombre_ejercicio(equip)}\n"
    texto += f"   • Lesiones: {lesion.title()}\n"
    texto += f"   • Fecha: {datetime.now().strftime('%d/%m/%Y')}\n\n"

    texto += "-" * 60 + "\n\n"

    for dia_info in rutina:
        dia = dia_info[0]
        ejercicios = dia_info[1]

        texto += f"DÍA {dia}\n"
        texto += "-" * 20 + "\n"

        for idx, (grupo, ejercicio) in enumerate(ejercicios, 1):
            texto += f"{idx}. {formatear_grupo_muscular(grupo)}: "
            texto += f"{formatear_nombre_ejercicio(ejercicio)}\n"

            # Agregar recomendación de series/reps si está disponible
            if len(dia_info) > 2:
                series = dia_info[2]
                reps = dia_info[3]
                texto += f"   └─ {series} series x {reps} repeticiones\n"

        texto += "\n"

    texto += "=" * 60 + "\n"
    texto += "💡 CONSEJOS:\n"
    texto += "   • Calienta 5-10 minutos antes de empezar\n"
    texto += "   • Descansa 60-90 segundos entre series\n"
    texto += "   • Mantén una técnica correcta\n"
    texto += "   • Hidrátate durante el entrenamiento\n"
    texto += "   • Progresa gradualmente el peso/intensidad\n"
    texto += "=" * 60 + "\n"

    return texto


def rutina_a_json(
    rutina: List, objetivo: str, nivel: str, dias: int, lesion: str, equip: str
) -> str:
    """
    Convierte una rutina a formato JSON.
    """
    data = {
        "configuracion": {
            "objetivo": objetivo,
            "nivel": nivel,
            "dias_semana": dias,
            "lesion": lesion,
            "equipamiento": equip,
            "fecha_generacion": datetime.now().isoformat(),
        },
        "rutina": [],
    }

    for dia_info in rutina:
        dia_data = {"dia": dia_info[0], "ejercicios": []}

        for grupo, ejercicio in dia_info[1]:
            ejercicio_data = {
                "grupo_muscular": grupo,
                "nombre": ejercicio,
                "nombre_formateado": formatear_nombre_ejercicio(ejercicio),
            }

            if len(dia_info) > 2:
                ejercicio_data["series"] = dia_info[2]
                ejercicio_data["repeticiones"] = dia_info[3]

            dia_data["ejercicios"].append(ejercicio_data)

        data["rutina"].append(dia_data)

    return json.dumps(data, indent=2, ensure_ascii=False)


def calcular_volumen_semanal(rutina: List) -> Dict[str, int]:
    """
    Calcula el volumen de entrenamiento por grupo muscular en la semana.
    """
    volumen = {"pecho": 0, "espalda": 0, "piernas": 0, "hombros": 0, "core": 0}

    for dia_info in rutina:
        ejercicios = dia_info[1]
        for grupo, _ in ejercicios:
            if grupo in volumen:
                volumen[grupo] += 1

    return volumen


def generar_recomendaciones_progresion(nivel: str, objetivo: str) -> List[str]:
    """
    Genera recomendaciones de progresión según nivel y objetivo.
    """
    recomendaciones = []

    if nivel == "principiante":
        recomendaciones.append("Enfócate en dominar la técnica antes de aumentar peso")
        recomendaciones.append(
            "Aumenta el peso cuando puedas hacer 2 reps extra con buena forma"
        )
        recomendaciones.append(
            "Descansa al menos 48h entre entrenamientos del mismo grupo muscular"
        )

    elif nivel == "intermedio":
        recomendaciones.append(
            "Implementa sobrecarga progresiva: +2.5-5% de peso cada 1-2 semanas"
        )
        recomendaciones.append("Varía el rango de repeticiones cada 4-6 semanas")
        recomendaciones.append(
            "Considera técnicas avanzadas como drop sets ocasionales"
        )

    else:  # avanzado
        recomendaciones.append(
            "Periodiza tu entrenamiento en fases de volumen/intensidad"
        )
        recomendaciones.append(
            "Monitorea tu fatiga y ajusta el volumen según sea necesario"
        )
        recomendaciones.append("Implementa deloads cada 4-6 semanas")

    if objetivo == "ganar_masa":
        recomendaciones.append("Asegura un superávit calórico de 300-500 kcal/día")
        recomendaciones.append("Consume 1.6-2.2g de proteína por kg de peso corporal")

    elif objetivo == "bajar_grasa":
        recomendaciones.append(
            "Mantén un déficit calórico moderado de 300-500 kcal/día"
        )
        recomendaciones.append(
            "Prioriza la retención de músculo con proteína alta (2-2.4g/kg)"
        )

    elif objetivo == "fuerza":
        recomendaciones.append(
            "Prioriza ejercicios compuestos con peso alto (80-90% 1RM)"
        )
        recomendaciones.append("Descansa 3-5 minutos entre series pesadas")

    return recomendaciones


def obtener_estadisticas_rutina(rutina: List) -> Dict[str, Any]:
    """
    Calcula estadísticas generales de la rutina.
    """
    total_ejercicios = sum(len(dia_info[1]) for dia_info in rutina)
    dias_entrenamiento = len(rutina)
    volumen = calcular_volumen_semanal(rutina)

    # Ejercicios por día
    ejercicios_por_dia = [len(dia_info[1]) for dia_info in rutina]
    promedio_ejercicios = (
        sum(ejercicios_por_dia) / len(ejercicios_por_dia) if ejercicios_por_dia else 0
    )

    return {
        "total_ejercicios": total_ejercicios,
        "dias_entrenamiento": dias_entrenamiento,
        "promedio_ejercicios_por_dia": round(promedio_ejercicios, 1),
        "volumen_por_grupo": volumen,
        "grupo_mas_trabajado": max(volumen.items(), key=lambda x: x[1])[0]
        if volumen
        else None,
    }


def formatear_tiempo(segundos: int) -> str:
    """
    Formatea segundos a formato MM:SS.
    """
    minutos = segundos // 60
    segs = segundos % 60
    return f"{minutos:02d}:{segs:02d}"


def parsear_prolog_output(prolog_result: List) -> List:
    """
    Parsea la salida de Prolog a un formato más manejable.
    """
    if not prolog_result:
        return []

    rutina = prolog_result[0].get("Rutina", [])
    return rutina


def generar_archivo_ics(
    rutina: List, objetivo: str, nivel: str, dias: int, lesion: str, equip: str
) -> str:
    """
    Genera un archivo .ics (iCalendar) para importar en cualquier calendario.
    """
    from datetime import datetime, timedelta

    ics_content = """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//GymBot//Rutina Entrenamiento//ES
CALSCALE:GREGORIAN
METHOD:PUBLISH
X-WR-CALNAME:Rutina GymBot
X-WR-TIMEZONE:America/Argentina/Buenos_Aires
X-WR-CALDESC:Plan de entrenamiento personalizado generado por GymBot

"""

    # Fecha de inicio (próximo lunes)
    hoy = datetime.now()
    dias_hasta_lunes = (7 - hoy.weekday()) % 7
    if dias_hasta_lunes == 0:
        dias_hasta_lunes = 7
    proximo_lunes = hoy + timedelta(days=dias_hasta_lunes)

    # Generar eventos para 4 semanas
    for semana in range(4):
        for dia_info in rutina:
            dia_num = dia_info[0]
            ejercicios = dia_info[1]

            # Calcular fecha del evento
            fecha_evento = proximo_lunes + timedelta(weeks=semana, days=dia_num - 1)
            fecha_inicio = fecha_evento.replace(hour=8, minute=0, second=0)
            fecha_fin = fecha_evento.replace(hour=9, minute=30, second=0)

            # Formatear fechas para iCalendar
            dtstart = fecha_inicio.strftime("%Y%m%dT%H%M%S")
            dtend = fecha_fin.strftime("%Y%m%dT%H%M%S")
            dtstamp = datetime.now().strftime("%Y%m%dT%H%M%SZ")

            # Crear descripción del evento
            descripcion = f"Objetivo: {objetivo.replace('_', ' ').title()}\\n"
            descripcion += f"Nivel: {nivel.title()}\\n\\n"
            descripcion += "Ejercicios:\\n"

            for grupo, ejercicio in ejercicios:
                ejercicio_formateado = formatear_nombre_ejercicio(ejercicio)
                descripcion += f"- {grupo.upper()}: {ejercicio_formateado}\\n"

            if len(dia_info) > 2:
                descripcion += f"\\n{dia_info[2]} series x {dia_info[3]} repeticiones"

            # Crear evento
            uid = f"gymbot-{semana}-{dia_num}-{fecha_inicio.strftime('%Y%m%d')}@gymbot.app"

            evento = f"""BEGIN:VEVENT
DTSTART:{dtstart}
DTEND:{dtend}
DTSTAMP:{dtstamp}
UID:{uid}
SUMMARY:💪 GymBot - Día {dia_num} (Semana {semana + 1})
DESCRIPTION:{descripcion}
STATUS:CONFIRMED
SEQUENCE:0
BEGIN:VALARM
TRIGGER:-PT30M
DESCRIPTION:Entrenamiento en 30 minutos
ACTION:DISPLAY
END:VALARM
END:VEVENT

"""
            ics_content += evento

    ics_content += "END:VCALENDAR"
    return ics_content


def calcular_1rm(peso: float, reps: int, formula: str = "epley") -> Dict[str, float]:
    """
    Calcula el 1RM (One Rep Max) usando diferentes fórmulas.

    Args:
        peso: Peso levantado
        reps: Repeticiones realizadas
        formula: 'epley', 'brzycki', 'lander', 'lombardi', 'mayhew', 'oconner', 'wathan'

    Returns:
        Dict con 1RM y porcentajes de entrenamiento
    """
    if reps == 1:
        one_rm = peso
    elif formula == "epley":
        one_rm = peso * (1 + reps / 30)
    elif formula == "brzycki":
        one_rm = peso * (36 / (37 - reps))
    elif formula == "lander":
        one_rm = (100 * peso) / (101.3 - 2.67123 * reps)
    elif formula == "lombardi":
        one_rm = peso * (reps**0.10)
    elif formula == "mayhew":
        one_rm = (100 * peso) / (52.2 + 41.9 * (2.718281828 ** (-0.055 * reps)))
    elif formula == "oconner":
        one_rm = peso * (1 + 0.025 * reps)
    elif formula == "wathan":
        one_rm = (100 * peso) / (48.8 + 53.8 * (2.718281828 ** (-0.075 * reps)))
    else:
        one_rm = peso * (1 + reps / 30)  # Default a Epley

    # Calcular porcentajes de entrenamiento
    porcentajes = {
        "1rm": round(one_rm, 1),
        "95": round(one_rm * 0.95, 1),
        "90": round(one_rm * 0.90, 1),
        "85": round(one_rm * 0.85, 1),
        "80": round(one_rm * 0.80, 1),
        "75": round(one_rm * 0.75, 1),
        "70": round(one_rm * 0.70, 1),
        "65": round(one_rm * 0.65, 1),
        "60": round(one_rm * 0.60, 1),
    }

    return porcentajes


def comparar_rutinas(rutina1: List, rutina2: List) -> Dict[str, Any]:
    """
    Compara dos rutinas y retorna las diferencias.
    """
    vol1 = calcular_volumen_semanal(rutina1)
    vol2 = calcular_volumen_semanal(rutina2)

    # Calcular diferencias de volumen
    diferencias_volumen = {}
    for grupo in vol1.keys():
        diferencias_volumen[grupo] = vol2[grupo] - vol1[grupo]

    # Ejercicios únicos de cada rutina
    ejercicios1 = set()
    ejercicios2 = set()

    for dia in rutina1:
        for _, ejercicio in dia[1]:
            ejercicios1.add(ejercicio)

    for dia in rutina2:
        for _, ejercicio in dia[1]:
            ejercicios2.add(ejercicio)

    solo_rutina1 = ejercicios1 - ejercicios2
    solo_rutina2 = ejercicios2 - ejercicios1
    comunes = ejercicios1 & ejercicios2

    return {
        "volumen_rutina1": vol1,
        "volumen_rutina2": vol2,
        "diferencias_volumen": diferencias_volumen,
        "ejercicios_solo_rutina1": list(solo_rutina1),
        "ejercicios_solo_rutina2": list(solo_rutina2),
        "ejercicios_comunes": list(comunes),
        "total_ejercicios_r1": len(ejercicios1),
        "total_ejercicios_r2": len(ejercicios2),
    }


def optimizar_secuencia_np(
    ejercicios: List[Tuple[str, str]], metodo: str = "greedy"
) -> Dict:
    """
    Llama al optimizador NP de Prolog para encontrar la secuencia óptima.

    Args:
        ejercicios: Lista de tuplas (grupo_muscular, nombre_ejercicio)
        metodo: 'greedy', 'branch_and_bound', o 'exacto'

    Returns:
        Dict con secuencia optimizada, costo, y estadísticas
    """
    from pyswip import Prolog

    # Extraer solo los nombres de ejercicios
    nombres_ejercicios = [ej[1] for ej in ejercicios]

    # Convertir a formato Prolog
    lista_prolog = "[" + ", ".join(nombres_ejercicios) + "]"

    try:
        prolog = Prolog()

        # Cargar la base de conocimiento
        import os

        base_dir = os.path.dirname(os.path.abspath(__file__))
        kb_dir = os.path.join(base_dir, "..", "prolog_kb")
        kb_dir = os.path.normpath(kb_dir).replace("\\", "/")

        list(prolog.query(f"working_directory(_, '{kb_dir}')"))
        prolog.consult("np_optimizer.pl")

        # Ejecutar consulta según el método
        if metodo == "greedy":
            query = f"resolver_greedy({lista_prolog}, Secuencia, Costo)"
        elif metodo == "branch_and_bound":
            query = f"resolver_branch_and_bound({lista_prolog}, Secuencia, Costo)"
        elif metodo == "exacto":
            query = f"resolver_exacto({lista_prolog}, Secuencia, Costo)"
        else:
            return {"error": f"Método inválido: {metodo}"}

        result = list(prolog.query(query))

        if result:
            secuencia_optimizada = result[0]["Secuencia"]
            costo_optimizado = result[0]["Costo"]

            # Calcular costo de la secuencia original para mostrar el ahorro real
            try:
                query_original = f"costo_secuencia({lista_prolog}, CostoOriginal)"
                print(f"🔍 Query costo original: {query_original}")
                result_original = list(prolog.query(query_original))
                print(f"🔍 Resultado query: {result_original}")
                costo_original = (
                    result_original[0]["CostoOriginal"]
                    if result_original
                    else costo_optimizado
                )
                print(f"✅ Costo original calculado: {costo_original}")
            except Exception as e:
                print(f"⚠️ No se pudo calcular costo original: {e}")
                import traceback

                traceback.print_exc()
                costo_original = costo_optimizado  # Fallback: asumir que no hay mejora

            # Calcular ahorro real
            ahorro = costo_original - costo_optimizado

            return {
                "success": True,
                "metodo": metodo,
                "secuencia_original": nombres_ejercicios,
                "secuencia_optimizada": secuencia_optimizada,
                "costo_original": costo_original,
                "costo_optimizado": costo_optimizado,
                "ahorro": ahorro,
                "costo": costo_optimizado,  # Mantener por compatibilidad
                "mejora": calcular_mejora_secuencia(
                    nombres_ejercicios, secuencia_optimizada
                ),
            }
        else:
            return {"error": "No se pudo optimizar la secuencia"}

    except Exception as e:
        return {"error": f"Error ejecutando optimizador: {str(e)}"}


def calcular_mejora_secuencia(original: List[str], optimizada: List[str]) -> str:
    """
    Calcula cuántos cambios se hicieron en la secuencia.
    """
    cambios = sum(
        1
        for i, ej in enumerate(original)
        if i < len(optimizada) and ej != optimizada[i]
    )
    total = len(original)
    porcentaje = (cambios / total) * 100 if total > 0 else 0

    return f"{cambios} ejercicios reordenados ({porcentaje:.1f}% de cambios)"
