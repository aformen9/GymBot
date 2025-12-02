import os
from flask import Flask, render_template, request, Response
from pyswip import Prolog

app = Flask(__name__)

# Parámetros de entrenamiento según objetivo
TRAINING_PARAMS = {
    "ganar_masa": {"series": 4, "reps": "8-12"},
    "bajar_grasa": {"series": 3, "reps": "15-20"},
    "fuerza": {"series": 5, "reps": "3-5"},
}


def get_training_params(objetivo):
    return TRAINING_PARAMS.get(
        objetivo,
        {"series": 3, "reps": "10-15"},  # default
    )


def consultar_rutina(objetivo, nivel, dias, lesion, equip):
    prolog = Prolog()

    base_dir = os.path.dirname(__file__)
    kb_path = os.path.join(base_dir, "..", "prolog_kb", "gym_kb.pl")
    kb_path = os.path.abspath(kb_path).replace("\\", "/")

    prolog.consult(kb_path)

    query = f"plan_rutina({objetivo}, {nivel}, {dias}, {lesion}, {equip}, Rutina)"
    result = list(prolog.query(query))

    if not result:
        return None

    return result[0]["Rutina"]


@app.route("/", methods=["GET", "POST"])
def index():
    rutina = None
    error = None

    # Para recordar lo que eligió el usuario y mostrar resumen
    objetivo = None
    nivel = None
    dias_int = None
    lesion = None
    equip = None
    training_params = None

    if request.method == "POST":
        objetivo = request.form.get("objetivo", "")
        nivel = request.form.get("nivel", "")
        dias = request.form.get("dias", "")
        lesion = request.form.get("lesion", "")
        equip = request.form.get("equipamiento", "")

        if not (objetivo and nivel and dias and lesion and equip):
            error = "Completá todos los campos."
        else:
            try:
                dias_int = int(dias)
                if dias_int < 1 or dias_int > 6:
                    error = "Los días deben estar entre 1 y 6."
                else:
                    rutina = consultar_rutina(objetivo, nivel, dias_int, lesion, equip)
                    if rutina is None:
                        error = "No se pudo generar una rutina con esos parámetros."
                    else:
                        training_params = get_training_params(objetivo)
            except ValueError:
                error = "Los días deben ser un número entero."

    return render_template(
        "index.html",
        rutina=rutina,
        error=error,
        objetivo=objetivo,
        nivel=nivel,
        dias=dias_int,
        lesion=lesion,
        equip=equip,
        training_params=training_params,
    )


@app.route("/download", methods=["POST"])
def download():
    objetivo = request.form.get("objetivo", "")
    nivel = request.form.get("nivel", "")
    dias = request.form.get("dias", "")
    lesion = request.form.get("lesion", "")
    equip = request.form.get("equipamiento", "")

    if not (objetivo and nivel and dias and lesion and equip):
        return Response("Faltan datos para generar la rutina.", mimetype="text/plain")

    try:
        dias_int = int(dias)
    except ValueError:
        return Response("Los días deben ser un número entero.", mimetype="text/plain")

    rutina = consultar_rutina(objetivo, nivel, dias_int, lesion, equip)
    if rutina is None:
        return Response(
            "No se pudo generar una rutina con esos parámetros.", mimetype="text/plain"
        )

    params = get_training_params(objetivo)

    # Construimos el texto plano
    lineas = []
    lineas.append("SisExpGymBot - Plan de entrenamiento\n")
    lineas.append(f"Objetivo: {objetivo}")
    lineas.append(f"Nivel: {nivel}")
    lineas.append(f"Días por semana: {dias_int}")
    lineas.append(f"Lesión: {lesion}")
    lineas.append(f"Equipamiento: {equip}")
    lineas.append("")
    lineas.append(
        f"Esquema: {params['series']} series de {params['reps']} reps por ejercicio"
    )
    lineas.append("")
    lineas.append("Plan semanal:")

    for dia in rutina:
        num_dia = dia[0]
        ejercicios = dia[1]
        lineas.append(f"\nDía {num_dia}:")
        for grupo, ejercicio in ejercicios:
            lineas.append(
                f"  - {grupo}: {ejercicio} ({params['series']} x {params['reps']})"
            )

    contenido = "\n".join(lineas)

    return Response(
        contenido,
        mimetype="text/plain",
        headers={"Content-Disposition": "attachment; filename=rutina_gymbot.txt"},
    )


if __name__ == "__main__":
    app.run(debug=True)
