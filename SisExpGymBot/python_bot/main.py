import os
from pyswip import Prolog
from dialogue import preguntar_parametros_usuario


def generar_rutina_desde_prolog(objetivo, nivel, dias, lesion, equipamiento):
    prolog = Prolog()

    # Ruta absoluta al archivo principal de Prolog
    base_dir = os.path.dirname(__file__)
    kb_path = os.path.join(base_dir, "..", "prolog_kb", "gym_kb.pl")
    kb_path = os.path.abspath(kb_path).replace("\\", "/")

    print("\nCargando KB desde:", kb_path)
    prolog.consult(kb_path)

    print("\nConsultando rutina semanal...")
    # armamos la consulta Prolog con los átomos elegidos por el usuario
    query = (
        f"plan_rutina({objetivo}, {nivel}, {dias}, {lesion}, {equipamiento}, Rutina)"
    )

    result = list(prolog.query(query))

    if not result:
        print("Prolog no pudo generar una rutina.")
        return

    rutina = result[0]["Rutina"]
    print("Rutina devuelta por Prolog:", rutina)

    # Mostrar más lindo
    print("\n=== PLAN DE ENTRENAMIENTO SEMANAL ===")
    print(f"Objetivo:     {objetivo}")
    print(f"Nivel:        {nivel}")
    print(f"Días/semana:  {dias}")
    print(f"Lesión:       {lesion}")
    print(f"Equipamiento: {equipamiento}")

    for dia in rutina:
        # dia = [numero_dia, lista_de_[grupo,ejercicio]]
        dia_num = dia[0]
        ejercicios = dia[1]

        print(f"\nDía {dia_num}:")
        for grupo, ejercicio in ejercicios:
            print(f"  {grupo}: {ejercicio}")


if __name__ == "__main__":
    print("Bienvenido a SisExpGymBot\n")
    params = preguntar_parametros_usuario()
    objetivo, nivel, dias, lesion, equip = params
    generar_rutina_desde_prolog(objetivo, nivel, dias, lesion, equip)
