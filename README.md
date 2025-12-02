# GymBot - Sistema Experto de Rutinas de Gimnasio

GymBot es un sistema híbrido (Python + Prolog) que genera rutinas de entrenamiento personalizadas basadas en objetivos, nivel de experiencia, equipamiento disponible y lesiones preexistentes.

## Arquitectura

* **Lógica (Backend):** SWI-Prolog (Inferencia y reglas expertas).
* **Interfaz (Frontend):** Flask (Web) y Python CLI.
* **Integración:** PySwip (Puente Python-Prolog).

## Requisitos Previos

1.  **Python 3.8+**
2.  **SWI-Prolog:** Es necesario tenerlo instalado en el sistema para que PySwip funcione.
    * **Mac (Homebrew):** `brew install swi-prolog`
    * **Linux:** `sudo apt-get install swi-prolog`
    * **Windows:** Descargar desde [swi-prolog.org](https://www.swi-prolog.org/Download.html) y agregar al PATH.

## Instalación

1.  Clonar el repositorio.
2.  Instalar dependencias de Python:
    ```bash
    pip install -r requirements.txt
    ```

## Uso

### Interfaz Web (Recomendado)
```bash
cd python_bot
python web_app.py
# Abrir navegador en http://localhost:5000
