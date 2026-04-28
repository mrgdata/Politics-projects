"""
Sevilla siglo XIX - Visualizaciones
Scatter plots de densidad, scatter espacial, evolución población histórica
"""

import pandas as pd
from matplotlib import pyplot as plt
from matplotlib.lines import Line2D

from importlib import import_module

_df = import_module("00_defaults")

_dp = import_module("01_data_prep")
load_and_clean_data = _dp.load_and_clean_data
_sa = import_module("02_spatial_analysis")
SpatialAutoCorrCalculator = _sa.SpatialAutoCorrCalculator


def plot_spatial_and_density(df_parishes):
    """Genera scatter plots espaciales y de densidad."""
    sac_calc = SpatialAutoCorrCalculator(df_parishes)
    sac_calc.create_spatial_scatter(
        size_col="Población",
        color_col="Vecinos_Corrales_Pct",
        title="Porcentaje de población viviendo en corrales por parroquia (tamaño=población total)",
    )

    sac_calc.create_spatial_scatter(
        size_col="Casas",
        color_col="Corrales_Pct",
        title="Porcentaje de corrales del total de viviendas por parroquia (tamaño=nº viviendas)",
    )

    sac_calc.create_flexible_scatter(
        "Households_Density", "Corrales_Density", "Población"
    )


def plot_historical_population():
    """Evolución de la población de Sevilla (1799-1920)."""
    data = {
        1799: [80598, "González de León"],
        1821: [75000, "Censo del Trienio"],
        1832: [96683, "Guía de Herrera Dávila"],
        1857: [112529, "dummy", 34.89],
        1860: [118298, "Censo Estatal"],
        1877: [134318, "Censo Estatal", 36.78],
        1887: [143182, "Censo Estatal"],
        1897: [145728, "Censo Estatal"],
        1900: [148315, "Censo Estatal", 37.42],
        1910: [158287, "Censo Estatal"],
        1920: [205529, "Censo Estatal"],
    }

    rows = []
    for year, vals in data.items():
        pop = vals[0]
        source = vals[1]
        pct_u20 = vals[2] if len(vals) > 2 else None
        rows.append(
            {"Year": year, "Population": pop, "Source": source, "Pct_Under20": pct_u20}
        )

    df = pd.DataFrame(rows)
    df["Pop_Under20"] = df["Population"] * df["Pct_Under20"] / 100

    def categorize_source(row):
        if "Censo Estatal" not in row["Source"]:
            return "pre-Censo Estatal"
        elif 1800 <= row["Year"] <= 1901:
            return "Censo Estatal (S. XIX)"
        else:
            return "Censo Estatal (S. XX)"

    df["Source Type"] = df.apply(categorize_source, axis=1)

    fig, ax = plt.subplots(figsize=(14, 7))

    color_map = {
        "pre-Censo Estatal": "gray",
        "Censo Estatal (S. XIX)": "tab:red",
        "Censo Estatal (S. XX)": "tab:green",
    }
    for i in range(len(df) - 1):
        x_seg, y_seg = df["Year"].iloc[i : i + 2], df["Population"].iloc[i : i + 2]
        ax.plot(
            x_seg,
            y_seg,
            color=color_map[df["Source Type"].iloc[i + 1]],
            lw=2.5,
            marker="o",
            markersize=5,
        )

    df_u20 = df.dropna(subset=["Pop_Under20"])
    ax.plot(
        df_u20["Year"],
        df_u20["Pop_Under20"],
        color="orange",
        linestyle="--",
        lw=2,
        marker="s",
        markersize=7,
        label="Población < 20 años (Calculada)",
    )

    for i, row in df_u20.iterrows():
        ax.annotate(
            f"{row['Pct_Under20']}%",
            (row["Year"], row["Pop_Under20"]),
            textcoords="offset points",
            xytext=(0, 10),
            ha="center",
            color="darkorange",
            fontweight="bold",
            fontsize=9,
        )

    boundary_years = [1814, 1837, 1873]
    for year in boundary_years:
        ax.axvline(x=year, color="black", linestyle=":", alpha=0.3)

    periods = [
        (1799, 1814, "Carlos IV y\nGuerra Indep."),
        (1814, 1837, "Fernando VII"),
        (1837, 1873, "Isabel II"),
        (1873, 1920, "Restauración"),
    ]

    y_top = ax.get_ylim()[1]
    for start, end, label in periods:
        mid = start + (end - start) / 2
        ax.text(
            mid,
            y_top * 0.93,
            label,
            ha="center",
            fontsize=9,
            style="italic",
            bbox=dict(facecolor="white", alpha=0.6, edgecolor="none"),
        )

    ax.set_title(
        "Evolución de la Población Total y de jóvenes en Sevilla", fontsize=16, pad=25
    )
    ax.set_ylabel("Número de Habitantes")
    ax.set_xlabel("Año")
    ax.grid(True, axis="y", linestyle="--", alpha=0.5)

    legend_elements = [
        Line2D([0], [0], color="gray", lw=3, label="Fuente: Historiadores (pre-Censo)"),
        Line2D([0], [0], color="tab:red", lw=3, label="Fuente: Censo Estatal S. XIX"),
        Line2D([0], [0], color="tab:green", lw=3, label="Fuente: Censo Estatal S. XX"),
        Line2D(
            [0],
            [0],
            color="orange",
            lw=2,
            linestyle="--",
            marker="s",
            label="Población < 20 años",
        ),
    ]
    ax.legend(handles=legend_elements, loc="lower right", title="Categorías de Datos")

    plt.tight_layout()
    plt.savefig(f"{_df.DIR_DATA}/seville_population_evolution.png")
    plt.show()


if __name__ == "__main__":
    df = load_and_clean_data()
    if df is not None:
        plot_spatial_and_density(df)
    plot_historical_population()
