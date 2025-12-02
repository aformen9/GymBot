import sys
import os
import webbrowser
from threading import Timer
from flask import Flask, render_template, request, Response
from pyswip import Prolog
from werkzeug.middleware.proxy_fix import ProxyFix


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
app.wsgi_app = ProxyFix(app.wsgi_app, x_for=1, x_proto=1, x_host=1, x_prefix=1)

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

            # 3. CARGA MANUAL DE ARCHIVOS (Aquí estaba el error antes)
            # Cargamos exercises primero
            path_ex = "exercises.pl"
            print(f"📄 Cargando {path_ex}...")
            prolog.consult(path_ex)

            # Cargamos rules después
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
                f"🧠 Consultando: plan_rutina({objetivo}, {nivel}, {dias}, {lesion}, {equip}, R)"
            )
            query = (
                f"plan_rutina({objetivo}, {nivel}, {dias}, {lesion}, {equip}, Rutina)"
            )
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
    objetivo, nivel, dias_int, lesion, equip = "", "", 3, "", ""

    if request.method == "POST":
        try:
            objetivo = request.form.get("objetivo")
            nivel = request.form.get("nivel")
            dias = request.form.get("dias")
            lesion = request.form.get("lesion")
            equip = request.form.get("equipamiento")
            dias_int = int(dias)

            rutina = consultar_rutina(objetivo, nivel, dias_int, lesion, equip)
            params = {"series": 4, "reps": "8-12"}

        except Exception as e:
            print(f"Error: {e}")
            error = "Error procesando datos."

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
    )


@app.route("/download", methods=["POST"])
def download():
    rutina_txt = request.form.get("rutina_texto", "Rutina GymBot")
    return Response(
        f"PLAN GYMBOT\n\n{rutina_txt}",
        mimetype="text/plain",
        headers={"Content-Disposition": "attachment; filename=rutina.txt"},
    )


def open_browser():
    webbrowser.open_new("http://127.0.0.1:8080")


if __name__ == "__main__":
    Timer(1.5, open_browser).start()
    app.run(port=8080, threaded=False)
