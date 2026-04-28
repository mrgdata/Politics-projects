"""
Sevilla siglo XIX - Análisis espacial
Clase SpatialAutoCorrCalculator: I de Moran, scatter espacial
"""

import pandas as pd
import numpy as np
from esda.moran import Moran
from libpysal.weights import KNN
from matplotlib import pyplot as plt


class SpatialAutoCorrCalculator:
    """Calcula medidas de autocorrelación espacial a partir de coordenadas."""

    def __init__(self, dataframe, lat_col="Lat_Center", lon_col="Lon_Center", k_neighbors=3):
        self.df = dataframe.dropna(subset=[lat_col, lon_col, "Parroquia"]).copy()
        coords = self.df[[lat_col, lon_col]].values
        self.weights = KNN.from_array(coords, k=k_neighbors)
        self.weights.transform = "R"
        self.parish_names = self.df["Parroquia"].tolist()

    def get_morans_i(self, data_column, p_value_threshold=0.05):
        """Calcula e interpreta la I de Moran global."""
        if data_column not in self.df.columns:
            raise ValueError(f"Columna '{data_column}' no encontrada.")

        data_values = pd.to_numeric(self.df[data_column]).values
        np.random.seed(42)
        mi = Moran(data_values, self.weights, permutations=999)

        interpretation = "Distribución ALEATORIA."
        significance = "No significativo estadísticamente."

        if mi.p_sim <= p_value_threshold:
            significance = f"Significativo (p-value: {mi.p_sim:.4f})."
            if mi.I > mi.EI_sim:
                interpretation = "AUTOCORRELACIÓN POSITIVA (Agrupada)."
            elif mi.I < mi.EI_sim:
                interpretation = "AUTOCORRELACIÓN NEGATIVA (Dispersa)."

        print("\n" + "=" * 60)
        print(f"Análisis I de Moran global para: '{data_column}'")
        print("=" * 60)
        print(f"{'Estadístico':<25} | {'Valor':<15}")
        print("-" * 45)
        print(f"{'Moran I Observado':<25} | {mi.I:^15.4f}")
        print(f"{'Moran I Esperado E[I]':<25} | {mi.EI_sim:^15.4f}")
        print(f"{'Z-score (simulado)':<25} | {mi.z_sim:^15.4f}")
        print(f"{'p-value (simulado)':<25} | {mi.p_sim:^15.4f}")
        print("-" * 45)
        print(f"Significación: {significance}")
        print(f"Resultado: {interpretation}")
        print("=" * 60 + "\n")

        return mi.I, mi.p_sim

    def create_spatial_scatter(self, size_col, color_col, title="Distribución espacial parroquias de Sevilla"):
        """Scatter plot espacial con tamaño y color."""
        plt.figure(figsize=(12, 8))
        sizes = pd.to_numeric(self.df[size_col])
        colors = pd.to_numeric(self.df[color_col])
        display_sizes = (sizes / sizes.max()) * 1000

        scatter = plt.scatter(
            self.df["Lon_Center"], self.df["Lat_Center"],
            s=display_sizes, c=colors, cmap="RdYlGn_r",
            alpha=0.7, edgecolors="black",
        )

        for i, txt in enumerate(self.df["Parroquia"]):
            plt.annotate(txt, (self.df["Lon_Center"].iloc[i], self.df["Lat_Center"].iloc[i]),
                         fontsize=8, alpha=0.8, xytext=(5, 5), textcoords="offset points")

        plt.colorbar(scatter, label=color_col)
        plt.xlabel("Longitud")
        plt.ylabel("Latitud")
        plt.title(title)
        plt.grid(True, linestyle="--", alpha=0.6)
        plt.show()

    def create_flexible_scatter(self, x_col, y_col, size_col=None, color_col=None):
        """Scatter plot flexible con tamaño y color opcionales."""
        plt.figure(figsize=(12, 8))
        plot_kwargs = {"alpha": 0.7, "edgecolors": "black"}

        if size_col and size_col in self.df.columns:
            sizes = pd.to_numeric(self.df[size_col], errors="coerce").fillna(0)
            plot_kwargs["s"] = (sizes / sizes.max()) * 1000 + 20
        else:
            plot_kwargs["s"] = 100

        if color_col and color_col in self.df.columns:
            plot_kwargs["c"] = pd.to_numeric(self.df[color_col], errors="coerce").fillna(0)
            plot_kwargs["cmap"] = "RdYlGn_r"
        else:
            plot_kwargs["c"] = "steelblue"

        scatter = plt.scatter(self.df[x_col], self.df[y_col], **plot_kwargs)

        if color_col:
            plt.colorbar(scatter, label=f"{color_col} (Rojo=Alto, Verde=Bajo)")

        plt.title("Relación entre la densidad de los corrales y del resto de viviendas en cada parroquia", fontsize=14)
        plt.suptitle("densidad = población / nº viviendas, tamaño=población total")
        plt.xlabel("Densidad del resto de viviendas")
        plt.ylabel("Densidad de los corrales")
        slope = 4
        plt.axline((0, 0), slope=slope, color="black", linestyle="--")
        plt.grid(True, linestyle="--", alpha=0.5)
        plt.text(9, 30, "Línea de proporción x4", verticalalignment="top", fontsize=10)

        if "Parroquia" in self.df.columns:
            for i, txt in enumerate(self.df["Parroquia"]):
                plt.annotate(txt, (self.df[x_col].iloc[i], self.df[y_col].iloc[i]),
                             fontsize=8, xytext=(5, 5), textcoords="offset points")
        plt.show()


if __name__ == "__main__":
    from importlib import import_module
    mod = import_module("01_data_prep")
    load_and_clean_data = mod.load_and_clean_data
    df = load_and_clean_data()
    if df is not None:
        sac = SpatialAutoCorrCalculator(df)
        sac.get_morans_i("Vecinos_Corrales_Pct")
        sac.get_morans_i("Corrales_Pct")
