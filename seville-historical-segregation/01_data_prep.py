"""
Sevilla siglo XIX - Preparación de datos
Carga de datos parroquiales, limpieza de formatos numéricos
"""

import pandas as pd
import numpy as np
import io
import os

# --- Datos crudos embebidos (CSV) ---
DIR_CSV_DATA = (
    f"C:/Users/{os.getenv('USERNAME', 'user').lower()}/Downloads/cuadro_corrales_sevilla.csv"
)

raw_csv_data = """Parroquia,Casas,Corrales (N-%_total_casas),Población,Vecinos_Corrales (N-%_total_poblacion),Lat_Center,Lon_Center
"Santa Ana y la O","1195","69-5,77","17468","5230-29,94",37.3857,-6.0022
"S. Andrés","223","8-3,58","1613","271-16,80",37.3934,-5.9926
"S. Bartolomé","215","10-4,60","2774","606-21,84",37.3875,-5.9877
"S. Bernardo","287","29-10,10","4513","2207-48,90",37.3831,-5.9808
"Santa Catalina","298","37-12,41","4505","2308-51,23",37.3923,-5.9871
"Santa Cruz","178","14-7,86","2091","1141-54,56",37.3853,-5.9904
"S. Esteban","179","8-4,46","1989","429-21,56",37.3905,-5.9868
"S. Gil","383","74-19,32","6120","3297-58,87",37.4014,-5.9899
"S. Ildefonso","187","5-2,67","1401","406-28,97",37.3904,-5.9893
"S. Isidoro","252","4-1,58","1851","313-16,90",37.3888,-5.9918
"S. Juan Bautista","331","23-6,94","3623","1982-54,70",37.3952,-5.9918
"S. Julián","153","24-15,60","1778","1442-81,10",37.3980,-5.9866
"S. Lorenzo","701","49-6,99","7614","2946-38,69",37.3970,-5.9961
"S. Lucía","146","18-12,32","2495","951-38,11",37.3965,-5.9850
"Magdalena","947","24-2,50","7122","994-13,95",37.3913,-5.9972
"S. Marcos","219","7-3,19","2625","446-12,90",37.3960,-5.9880
"Santa M. la Blanca","121","13-10,74","1350","837-62,00",37.3866,-5.9877
"Santa Marina","189","20-10,58","3254","1243-38,19",37.3982,-5.9889
"S. Martín","427","13-3,04","3328","971-29,17",37.3948,-5.9937
"S. Miguel","183","2-1,09","1599","141-8,81",37.3934,-5.9959
"S. Nicolás","98","8-8,33","13df58","787-57,95",37.3878,-5.9902
"Omnium Sactorum","807","38-4,70","8489","4175-49,18",37.3995,-5.9918
"S. Pedro","235","7-2,97","1891","679-35,90",37.3930,-5.9898
"S. Roque","431","92-21,30","7652","4240-55,41",37.3892,-5.9818
"S. Román","276","18-6,52","3618","1155-31,92",37.3942,-5.9876
"Sagrario","1980","42-2,12","14357","2138-14,89",37.3860,-5.9934
"Salvador","916","17-1,85","6076","1197-19,70",37.3895,-5.9931
"Santiago","153","19-12,41","2455","1495-60,89",37.3901,-5.9859
"S. Vicente","713","42-5,89","8149","2310-28,34",37.3963,-5.9984"""


def load_and_clean_data():
    """Carga y limpia los datos parroquiales."""
    try:
        if os.path.exists(DIR_CSV_DATA):
            df = pd.read_csv(DIR_CSV_DATA)
        else:
            print(f"Archivo CSV no encontrado en {DIR_CSV_DATA}. Usando datos embebidos.")
            df = pd.read_csv(io.StringIO(raw_csv_data))
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
        .astype(str).str.split("-").str[1]
        .str.replace(",", ".").astype(float)
    )
    df["Vecinos_Corrales_Count"] = (
        df["Vecinos_Corrales (N-%_total_poblacion)"]
        .astype(str).str.split("-").str[0]
        .str.replace(".", "", regex=False).astype(int)
    )
    df["Vecinos_Corrales_Pct"] = (
        df["Vecinos_Corrales (N-%_total_poblacion)"]
        .astype(str).str.split("-").str[1]
        .str.replace(".", "", regex=False)
        .str.replace(",", ".").astype(float)
    )
    df["Corrales_Density"] = round(df["Vecinos_Corrales_Count"] / df["Corrales_Count"], 2)
    df["Households_Density"] = round(
        (df["Población"] - df["Vecinos_Corrales_Count"])
        / (df["Casas"] - df["Corrales_Count"]), 2
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
        print(df[["Parroquia", "Población", "Corrales_Density", "Households_Density"]].head())
