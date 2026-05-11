"""
Sevilla siglo XIX - Visualizaciones
Scatter plots de densidad, scatter espacial, evolución población histórica,
estructura patrimonial y evolución de corrales
"""

import pandas as pd
import numpy as np
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
    data = _df.POPULATION_DATA

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

    boundary_years = [1814, 1837, 1874, 1914]
    for year in boundary_years:
        ax.axvline(x=year, color="black", linestyle=":", alpha=0.3)

    periods = [
        (1799, 1814, "Carlos IV y\nGuerra Indep."),
        (1814, 1837, "Fernando VII"),
        (1837, 1873, "Isabel II"),
        (1874, 1914, "Restauración"),
        (1914, 1920, "IGM"),
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
    plt.savefig(f"{_df.DIR_PLOT}/seville_population_evolution.png")
    plt.show()


def plot_patrimonio_evolution():
    """Evolución de la estructura patrimonial (Inventarios Post-Mortem, S.XVI vs S.XVII)."""
    df_comp = pd.DataFrame(
        [_df.PATRIMONIO_XVI, _df.PATRIMONIO_XVII], index=["Siglo XVI", "Siglo XVII"]
    ).T
    df_comp["Variación (%)"] = df_comp["Siglo XVII"] - df_comp["Siglo XVI"]

    fig, ax = plt.subplots(figsize=(12, 7))
    df_comp[["Siglo XVI", "Siglo XVII"]].plot(
        kind="bar", ax=ax, color=["#df4f4f", "#44d444"], edgecolor="black"
    )

    ax.set_title(
        "Evolución de la Estructura Patrimonial (Inventarios Post-Mortem)",
        fontsize=14, pad=20,
    )
    ax.set_ylabel("Porcentaje sobre el Total (%)")
    ax.set_xlabel("Categorías de Bienes")
    ax.grid(axis="y", linestyle="--", alpha=0.6)

    for p in ax.patches:
        ax.annotate(
            f"{p.get_height()}%",
            (p.get_x() + p.get_width() / 2.0, p.get_height()),
            ha="center", va="center", xytext=(0, 9),
            textcoords="offset points", fontsize=9,
        )

    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{_df.DIR_PLOT}/patrimonio_evolution.png")
    plt.show()

    print("Tabla Comparativa de Inventarios:")
    print(df_comp.sort_values(by="Variación (%)", ascending=False))


def plot_wealth_distribution():
    """Distribución de bienes muebles e inmuebles por tramos de riqueza (S.XVI)."""
    categories = list(_df.WEALTH_CATEGORIES)
    muebles = list(_df.WEALTH_MUEBLES)
    inmuebles = list(_df.WEALTH_INMUEBLES)

    # Invertir para disposición visual (0/250.000 arriba)
    categories.reverse()
    muebles.reverse()
    inmuebles.reverse()

    fig, ax = plt.subplots(figsize=(10, 8))

    bar_width = 0.6
    indices = np.arange(len(categories))

    ax.barh(indices, muebles, bar_width, label="Muebles",
            color="#df4f4f", edgecolor="black", linewidth=0.5)
    ax.barh(indices, inmuebles, bar_width, left=muebles, label="Inmuebles",
            color="#44d444", edgecolor="black", linewidth=0.5)

    for i, (m, inm) in enumerate(zip(muebles, inmuebles)):
        ax.text(m / 2, i, f"{m:.2f}%", ha="center", va="center",
                color="black", fontsize=9, fontweight="bold")
        ax.text(m + inm / 2, i, f"{inm:.2f}%", ha="center", va="center",
                color="black", fontsize=9, fontweight="bold")

    ax.set_title(
        "Distribución de bienes muebles e inmuebles en el patrimonio post-morten",
        pad=20, fontsize=12, fontweight="bold",
    )
    plt.suptitle(
        "Sevilla, Siglo XVI (en maravedíes). Inmuebles: Censos, juros y fincas rústicas y urbanas"
    )
    ax.set_yticks(indices)
    ax.set_yticklabels(categories, fontsize=10)
    ax.set_xticks([0, 25, 50, 75, 100])
    ax.set_xticklabels(["0,00%", "25,00%", "50,00%", "75,00%", "100,00%"], fontsize=10)
    ax.grid(axis="x", linestyle="-", alpha=0.3)
    ax.set_axisbelow(True)
    ax.legend(loc="upper center", bbox_to_anchor=(0.7, -0.05), ncol=2, frameon=False)
    plt.figtext(0.05, 0.05, "Total de los bienes", ha="left", fontsize=10)

    plt.tight_layout()
    plt.savefig(f"{_df.DIR_PLOT}/wealth_distribution_xvi.png")
    plt.show()


def plot_corrales_evolution():
    """Evolución del número de corrales y sus habitantes."""
    df_comp = pd.DataFrame(
        [_df.CORRALES_EVOLUCION_VECINOS, _df.CORRALES_EVOLUCION_CORRALES],
        index=["Vecinos (en cientos)", "Corrales"],
    ).T

    fig, ax = plt.subplots(figsize=(12, 7))
    df_comp[["Vecinos (en cientos)", "Corrales"]].plot(
        kind="bar", ax=ax, color=["#df4f4f", "#44d444"], edgecolor="black"
    )

    ax.set_title("Evolución del número de corrales y sus habitantes", fontsize=14, pad=20)
    ax.set_ylabel("Total")
    ax.grid(axis="y", linestyle="--", alpha=0.6)

    for p in ax.patches:
        ax.annotate(
            f"{p.get_height()}",
            (p.get_x() + p.get_width() / 2.0, p.get_height()),
            ha="center", va="center", xytext=(0, 9),
            textcoords="offset points", fontsize=9,
        )

    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{_df.DIR_PLOT}/corrales_evolution.png")
    plt.show()


if __name__ == "__main__":
    df = load_and_clean_data()
    if df is not None:
        plot_spatial_and_density(df)
    plot_historical_population()
    plot_patrimonio_evolution()
    plot_wealth_distribution()
    plot_corrales_evolution()
