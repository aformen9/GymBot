def elegir_opcion(texto_pregunta, opciones):
    """
    opciones: dict {numero: valor_prolog}
    Devuelve el valor_prolog elegido.
    """
    while True:
        print(texto_pregunta)
        for num, val in opciones.items():
            print(f"  {num}) {val}")
        eleccion = input("Elegí una opción: ").strip()
        if not eleccion.isdigit():
            print("Ingresá un número válido.\n")
            continue
        eleccion = int(eleccion)
        if eleccion in opciones:
            return opciones[eleccion]
        else:
            print("Opción inválida.\n")


def preguntar_parametros_usuario():
    """
    Devuelve una tupla:
      (objetivo, nivel, dias, lesion, equipamiento)
    todos como átomos Prolog (strings sin comillas en la consulta)
    """

    objetivos = {
        1: "ganar_masa",
        2: "bajar_grasa",
        3: "fuerza"
    }

    niveles = {
        1: "principiante",
        2: "intermedio",
        3: "avanzado"
    }

    equipamientos = {
        1: "peso_corporal",
        2: "mancuernas",
        3: "gym_completo"
    }

    lesiones = {
        1: "ninguna",
        2: "espalda",
        3: "rodilla",
        4: "hombro"
    }

    objetivo = elegir_opcion("¿Cuál es tu objetivo principal?", objetivos)
    nivel = elegir_opcion("¿Cuál es tu nivel actual?", niveles)
    equip = elegir_opcion("¿Con qué equipamiento contás?", equipamientos)
    lesion = elegir_opcion("¿Tenés alguna lesión relevante?", lesiones)

    # Preguntar días de entrenamiento
    while True:
        dias_str = input("¿Cuántos días por semana querés entrenar? (1 a 6): ").strip()
        if not dias_str.isdigit():
            print("Ingresá un número válido.\n")
            continue
        dias = int(dias_str)
        if 1 <= dias <= 6:
            break
        else:
            print("Ingresá un número entre 1 y 6.\n")

    return objetivo, nivel, dias, lesion, equip
