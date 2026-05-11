"""
Sevilla siglo XIX - Preparación de datos
Carga de datos parroquiales, limpieza de formatos numéricos
"""

import pandas as pd
import numpy as np
import io
import os
from importlib import import_module

_df = import_module("00_defaults")


# --- Datos crudos embebidos (CSV) ---
DIR_CSV_DATA = _df.DIR_DATA + _df.FILENAME_CSV
raw_csv_data = _df.RAW_CSV_DATA


def load_and_clean_data():
    """Carga y limpia los datos parroquiales."""
    if not os.path.exists(_df.DIR_DATA):
        os.makedirs(_df.DIR_DATA)
    try:
        if os.path.exists(DIR_CSV_DATA):
            df = pd.read_csv(DIR_CSV_DATA)
        else:
            print(
                f"Archivo CSV no encontrado en {DIR_CSV_DATA}. Usando datos embebidos."
            )
            df = pd.read_csv(io.StringIO(raw_csv_data))
            df.to_csv(DIR_CSV_DATA)
    except Exception as e:
        print(f"Error al leer datos CSV: {e}")
        return None

    print("Datos cargados correctamente.")

    # Limpieza de formatos numéricos
    df["Corrales_Count"] = (
        df["Corrales (N-%_total_casas)"].astype(str).str.split("-").str[0].astype(int)
    )
    df["Corrales_Pct"] = (
        df["Corrales (N-%_total_casas)"]
        .astype(str)
        .str.split("-")
        .str[1]
        .str.replace(",", ".")
        .astype(float)
    )
    df["Vecinos_Corrales_Count"] = (
        df["Vecinos_Corrales (N-%_total_poblacion)"]
        .astype(str)
        .str.split("-")
        .str[0]
        .str.replace(".", "", regex=False)
        .astype(int)
    )
    df["Vecinos_Corrales_Pct"] = (
        df["Vecinos_Corrales (N-%_total_poblacion)"]
        .astype(str)
        .str.split("-")
        .str[1]
        .str.replace(".", "", regex=False)
        .str.replace(",", ".")
        .astype(float)
    )
    df["Corrales_Density"] = round(
        df["Vecinos_Corrales_Count"] / df["Corrales_Count"], 2
    )
    df["Households_Density"] = round(
        (df["Población"] - df["Vecinos_Corrales_Count"])
        / (df["Casas"] - df["Corrales_Count"]),
        2,
    )
    df["Households_Density_log"] = np.log(df["Households_Density"]) + 1
    df["Corrales_Density_log"] = np.log(df["Corrales_Density"]) + 1
    df["Density Factor"] = df["Corrales_Density"] / df["Households_Density"]
    df["Density Factor_log"] = np.log(df["Density Factor"]) + 1

    df_parishes = df.dropna(subset=["Lat_Center", "Lon_Center"]).copy()
    return df_parishes


if __name__ == "__main__":
    df = load_and_clean_data()
    if df is not None:
        print(f"Datos listos: {len(df)} parroquias.")
        print(
            df[
                ["Parroquia", "Población", "Corrales_Density", "Households_Density"]
            ].head()
        )
