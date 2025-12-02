import sys
import os
import webbrowser
from threading import Timer
from flask import Flask, render_template, request, Response, jsonify, session
from pyswip import Prolog
from werkzeug.middleware.proxy_fix import ProxyFix
from datetime import datetime
import json

# Importar módulos propios
import utils
from routine_builder import RoutineBuilder


# --- 1. GESTIÓN DE RECURSOS ---
def resource_path(relative_path):
    try:
        base_path = sys._MEIPASS
    except Exception:
        base_path = os.path.abspath(".")
    return os.path.join(base_path, relative_path)


if getattr(sys, "frozen", False):
    template_folder = resource_path("templates")
else:
    template_folder = "templates"

app = Flask(__name__, template_folder=template_folder)
app.secret_key = "gymbot_secret_key_2024"  # Para sessions
app.wsgi_app = ProxyFix(app.wsgi_app, x_for=1, x_proto=1, x_host=1, x_prefix=1)

# Instanciar el constructor de rutinas
routine_builder = RoutineBuilder()

# --- 2. MOTOR PROLOG (CARGA MANUAL) ---
prolog_instance = None


def init_prolog():
    try:
        prolog = Prolog()

        # 1. Encontrar la carpeta prolog_kb
        if getattr(sys, "frozen", False):
            kb_dir = resource_path("prolog_kb")
        else:
            base_dir = os.path.dirname(os.path.abspath(__file__))
            kb_dir = os.path.join(base_dir, "..", "prolog_kb")

        kb_dir = os.path.normpath(kb_dir).replace("\\", "/")

        if os.path.exists(kb_dir):
            print(f"📂 Directorio KB encontrado: {kb_dir}")

            # 2. Configurar directorio de trabajo
            list(prolog.query(f"working_directory(_, '{kb_dir}')"))

            # 3. CARGA MANUAL DE ARCHIVOS
            path_ex = "exercises.pl"
            print(f"📄 Cargando {path_ex}...")
            prolog.consult(path_ex)

            path_rules = "rules.pl"
            print(f"📄 Cargando {path_rules}...")
            prolog.consult(path_rules)

            print("✅ TODOS LOS ARCHIVOS CARGADOS EXITOSAMENTE.")
            return prolog
        else:
            print(f"❌ No se encuentra la carpeta: {kb_dir}")

    except Exception as e:
        print(f"⚠️ Error crítico inicializando Prolog: {e}")
    return None


prolog_instance = init_prolog()


# --- 3. RESPALDO ---
def generar_respaldo(dias):
    print("⚠️ USANDO RESPALDO.")
    rutina = [
        (
            1,
            [
                ("pecho", "press_banca"),
                ("espalda", "remo_barra"),
                ("piernas", "sentadilla"),
                ("hombros", "press_militar"),
            ],
        ),
        (
            2,
            [
                ("pecho", "aperturas"),
                ("espalda", "jalon_polea"),
                ("piernas", "prensa"),
                ("core", "plancha"),
            ],
        ),
    ]
    res = []
    for i in range(1, dias + 1):
        res.append((i, rutina[(i - 1) % 2][1]))
    return res


def consultar_rutina(objetivo, nivel, dias, lesion, equip):
    if prolog_instance:
        try:
            print(
                f"🧠 Consultando: generar_rutina({objetivo}, {nivel}, {dias}, {lesion}, {equip}, R)"
            )
            query = f"generar_rutina({objetivo}, {nivel}, {dias}, {lesion}, {equip}, Rutina)"
            result = list(prolog_instance.query(query))

            if result:
                print("✅ Inferencia exitosa.")
                return result[0]["Rutina"]
            else:
                print("❌ Inferencia fallida (lista vacía).")
        except Exception as e:
            print(f"❌ Error Prolog: {e}")

    return generar_respaldo(dias)


# --- 4. RUTAS ---
@app.route("/", methods=["GET", "POST"])
def index():
    rutina = None
    error = None
    params = None
    stats = None
    recomendaciones = None
    objetivo, nivel, dias_int, lesion, equip = "", "", 3, "ninguna", ""

    if request.method == "POST":
        try:
            objetivo = request.form.get("objetivo")
            nivel = request.form.get("nivel")
            dias = request.form.get("dias")
            lesion = request.form.get("lesion")
            equip = request.form.get("equipamiento")
            dias_int = int(dias)

            # COMENTAR ESTAS LÍNEAS TEMPORALMENTE
            # es_valido, msg_error = utils.validar_parametros_rutina(
            #     objetivo, nivel, dias_int, lesion, equip
            # )
            #
            # if not es_valido:
            #     error = msg_error
            # else:

            # Consultar rutina desde Prolog
            rutina = consultar_rutina(objetivo, nivel, dias_int, lesion, equip)

            # Optimizar orden de ejercicios
            rutina = routine_builder.optimizar_orden_ejercicios(rutina, objetivo)

            # Obtener volumen recomendado
            params = {"series": 4, "reps": "8-12"}

            # Calcular estadísticas
            stats = utils.obtener_estadisticas_rutina(rutina)

            # Generar recomendaciones
            recomendaciones = utils.generar_recomendaciones_progresion(nivel, objetivo)

            # Guardar en sesión para historial
            if "historial" not in session:
                session["historial"] = []

            historial_entry = {
                "fecha": datetime.now().isoformat(),
                "objetivo": objetivo,
                "nivel": nivel,
                "dias": dias_int,
                "lesion": lesion,
                "equip": equip,
            }
            session["historial"].append(historial_entry)
            session.modified = True

        except Exception as e:
            print(f"Error: {e}")
            error = f"Error procesando datos: {str(e)}"

    return render_template(
        "index.html",
        rutina=rutina,
        error=error,
        objetivo=objetivo,
        nivel=nivel,
        dias=dias_int,
        lesion=lesion,
        equip=equip,
        training_params=params,
        stats=stats,
        recomendaciones=recomendaciones,
    )


@app.route("/download", methods=["POST"])
def download():
    try:
        objetivo = request.form.get("objetivo", "ganar_masa")
        nivel = request.form.get("nivel", "intermedio")
        dias = int(request.form.get("dias", 3))
        lesion = request.form.get("lesion", "ninguna")
        equip = request.form.get("equipamiento", "gym_completo")

        # Regenerar rutina para descargar
        rutina = consultar_rutina(objetivo, nivel, dias, lesion, equip)

        # Convertir a texto formateado
        rutina_texto = utils.rutina_a_texto(
            rutina, objetivo, nivel, dias, lesion, equip
        )

        return Response(
            rutina_texto,
            mimetype="text/plain",
            headers={"Content-Disposition": "attachment; filename=rutina_gymbot.txt"},
        )
    except Exception as e:
        return Response(
            f"Error generando descarga: {str(e)}", mimetype="text/plain", status=500
        )


@app.route("/download_json", methods=["POST"])
def download_json():
    try:
        objetivo = request.form.get("objetivo", "ganar_masa")
        nivel = request.form.get("nivel", "intermedio")
        dias = int(request.form.get("dias", 3))
        lesion = request.form.get("lesion", "ninguna")
        equip = request.form.get("equipamiento", "gym_completo")

        # Regenerar rutina
        rutina = consultar_rutina(objetivo, nivel, dias, lesion, equip)

        # Convertir a JSON
        rutina_json = utils.rutina_a_json(rutina, objetivo, nivel, dias, lesion, equip)

        return Response(
            rutina_json,
            mimetype="application/json",
            headers={"Content-Disposition": "attachment; filename=rutina_gymbot.json"},
        )
    except Exception as e:
        return Response(
            json.dumps({"error": str(e)}), mimetype="application/json", status=500
        )


@app.route("/historial")
def historial():
    """Muestra el historial de rutinas generadas."""
    hist = session.get("historial", [])
    return render_template("historial.html", historial=hist)


@app.route("/clear_historial", methods=["POST"])
def clear_historial():
    """Limpia el historial de rutinas."""
    session["historial"] = []
    session.modified = True
    return jsonify({"success": True})


@app.route("/api/ejercicio/<nombre>")
def info_ejercicio(nombre):
    """API endpoint para obtener información de un ejercicio."""
    from routine_builder import generar_notas_tecnica

    notas = generar_notas_tecnica(nombre)

    return jsonify(
        {"nombre": utils.formatear_nombre_ejercicio(nombre), "notas_tecnica": notas}
    )


@app.route("/deload", methods=["POST"])
def generar_deload():
    """Genera una semana de deload basada en la rutina actual."""
    try:
        objetivo = request.form.get("objetivo")
        nivel = request.form.get("nivel")
        dias = int(request.form.get("dias"))
        lesion = request.form.get("lesion")
        equip = request.form.get("equipamiento")

        # Obtener rutina normal
        rutina_normal = consultar_rutina(objetivo, nivel, dias, lesion, equip)

        # Generar deload
        from routine_builder import crear_plan_deload

        rutina_deload = crear_plan_deload(rutina_normal)

        return jsonify({"success": True, "rutina_deload": rutina_deload})
    except Exception as e:
        return jsonify({"success": False, "error": str(e)}), 500


def open_browser():
    webbrowser.open_new("http://127.0.0.1:8080")


@app.route("/download_calendar", methods=["POST"])
def download_calendar():
    """Genera y descarga archivo .ics para calendario."""
    try:
        objetivo = request.form.get("objetivo", "ganar_masa")
        nivel = request.form.get("nivel", "intermedio")
        dias = int(request.form.get("dias", 3))
        lesion = request.form.get("lesion", "ninguna")
        equip = request.form.get("equipamiento", "gym_completo")

        # Regenerar rutina
        rutina = consultar_rutina(objetivo, nivel, dias, lesion, equip)

        # Generar archivo ICS
        ics_content = utils.generar_archivo_ics(
            rutina, objetivo, nivel, dias, lesion, equip
        )

        return Response(
            ics_content,
            mimetype="text/calendar",
            headers={"Content-Disposition": "attachment; filename=rutina_gymbot.ics"},
        )
    except Exception as e:
        return Response(
            f"Error generando calendario: {str(e)}", mimetype="text/plain", status=500
        )


@app.route("/api/calcular_1rm", methods=["POST"])
def calcular_1rm_endpoint():
    """API endpoint para calcular 1RM."""
    try:
        data = request.get_json()
        peso = float(data.get("peso", 0))
        reps = int(data.get("reps", 1))
        formula = data.get("formula", "epley")

        if peso <= 0 or reps <= 0 or reps > 20:
            return jsonify({"error": "Valores inválidos"}), 400

        resultado = utils.calcular_1rm(peso, reps, formula)

        return jsonify(resultado)
    except Exception as e:
        return jsonify({"error": str(e)}), 500


@app.route("/api/comparar", methods=["POST"])
def comparar_rutinas_endpoint():
    """Compara dos rutinas del historial."""
    try:
        data = request.get_json()
        index1 = int(data.get("index1", 0))
        index2 = int(data.get("index2", 1))

        historial = session.get("historial", [])

        if len(historial) < 2:
            return jsonify(
                {"error": "Se necesitan al menos 2 rutinas en el historial"}
            ), 400

        if index1 >= len(historial) or index2 >= len(historial):
            return jsonify({"error": "Índices inválidos"}), 400

        # Obtener parámetros de ambas rutinas
        r1_params = historial[index1]
        r2_params = historial[index2]

        # Regenerar ambas rutinas
        rutina1 = consultar_rutina(
            r1_params["objetivo"],
            r1_params["nivel"],
            r1_params["dias"],
            r1_params["lesion"],
            r1_params["equip"],
        )

        rutina2 = consultar_rutina(
            r2_params["objetivo"],
            r2_params["nivel"],
            r2_params["dias"],
            r2_params["lesion"],
            r2_params["equip"],
        )

        # Comparar
        comparacion = utils.comparar_rutinas(rutina1, rutina2)

        return jsonify(comparacion)
    except Exception as e:
        return jsonify({"error": str(e)}), 500


if __name__ == "__main__":
    print("=" * 60)
    print("🏋️  GymBot - Sistema Experto de Entrenamiento")
    print("=" * 60)
    print("🌐 Servidor iniciando en http://127.0.0.1:8080")
    print("💡 Presiona Ctrl+C para detener el servidor")
    print("=" * 60)

    Timer(1.5, open_browser).start()
    app.run(port=8080, debug=False, threaded=True)
