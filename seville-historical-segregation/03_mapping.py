"""
Sevilla siglo XIX - Mapa interactivo con Folium
Mapa de parroquias coloreado por población
"""

import os
import webbrowser
from folium import Map, CircleMarker, Popup
from branca.colormap import linear

from importlib import import_module
_dp = import_module("01_data_prep")
load_and_clean_data = _dp.load_and_clean_data


def create_parish_map(df_parishes):
    """Genera un mapa interactivo Folium de las parroquias de Sevilla."""
    print("\nGenerando mapa interactivo...")

    seville_map = Map(location=[37.389, -5.992], zoom_start=15, tiles="OpenStreetMap")

    color_scale = linear.YlOrBr_05.scale(
        df_parishes["Población"].min(), df_parishes["Población"].max()
    )
    color_scale.caption = "Población Total por Parroquia (XIX C.)"
    color_scale.add_to(seville_map)

    for _, row in df_parishes.iterrows():
        point_color = color_scale(row["Población"])

        popup_text = (
            f"<b>Parroquia:</b> {row['Parroquia']}<br>"
            f"<b>Población:</b> {row['Población']}<br>"
            f"<b>Casas:</b> {row['Casas']}<br>"
            f"<b>Corrales (N-%_total):</b> {row['Corrales_Count']}-{row['Corrales_Pct']}<br>"
            f"<b>Población en corrales:</b> {row['Vecinos_Corrales_Count']}-{row['Vecinos_Corrales_Pct']}"
        )

        CircleMarker(
            location=[row["Lat_Center"], row["Lon_Center"]],
            radius=10,
            color="black",
            weight=1,
            fill=True,
            fill_color=point_color,
            fill_opacity=0.9,
            popup=Popup(popup_text, max_width=300),
            tooltip=f"{row['Parroquia']}: {row['Población']}",
        ).add_to(seville_map)

    map_filename = "seville_parish_population_map.html"
    seville_map.save(map_filename)
    print(f"Mapa guardado como '{map_filename}'.")

    try:
        webbrowser.open("file://" + os.path.realpath(map_filename))
    except Exception:
        print(f"No se pudo abrir el navegador. Abre '{map_filename}' manualmente.")


if __name__ == "__main__":
    df = load_and_clean_data()
    if df is not None:
        create_parish_map(df)
