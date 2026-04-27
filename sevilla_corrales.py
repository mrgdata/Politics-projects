"""
Seville 19th Century Spatial Analysis Script
This script:
1. Loads historical parish data.
2. Cleans the complex numerical formats.
3. Generates a visualization of Seville parishes on a map, color-coded by population.
4. Provides a method to calculate and interpret Global Moran's I (spatial autocorrelation).
"""

import pandas as pd
import numpy as np
import io
from folium import Map, CircleMarker, Popup
from esda.moran import Moran
from libpysal.weights import KNN
from branca.colormap import linear
import webbrowser
import os
from matplotlib import pyplot as plt
from matplotlib import cm as cm
from matplotlib.lines import Line2D

# --- STEP 0: RAW DATA (Paste the full CSV data here for a self-contained script) ---
# Note: Ensure the copy doesn't break string formatting. Re-copied here for a complete solution.
DIR_CSV_DATA = (
    f"C:/Users/{os.getenv('USERNAME').lower()}/Downloads/cuadro_corrales_sevilla.csv"
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


# =========================================================
# === CLASS DEFINITION: SPATIAL AUTO-CORRELATION TOOL ===
# =========================================================
class SpatialAutoCorrCalculator:
    """Calculates spatial statistical measures from a dataframe with coordinates."""

    def __init__(
        self, dataframe, lat_col="Lat_Center", lon_col="Lon_Center", k_neighbors=3
    ):
        """
        Initializes with spatial weights. Uses K-Nearest Neighbors (KNN) by default
        for point data when polygon boundaries are unavailable.
        """
        self.df = dataframe.dropna(subset=[lat_col, lon_col, "Parroquia"]).copy()

        # Define spatial weights: using KNN=3 as an appropriate neighbor count for
        # clustered parish points in a dense urban environment.
        coords = self.df[[lat_col, lon_col]].values
        # Note: libpysal accepts an array of shape (N, 2) which can be (lat, lon)
        self.weights = KNN.from_array(coords, k=k_neighbors)
        self.weights.transform = "R"  # Row-standardize weights
        self.parish_names = self.df["Parroquia"].tolist()

    def get_morans_i(self, data_column, p_value_threshold=0.05):
        """
        Task 4: Calculate and interpret Global Moran's I (Spatial Autocorrelation).

        Args:
            data_column (str): Name of the column in the dataframe to test.
            p_value_threshold (float): Significance limit (default: 0.05).

        Returns:
            A tuple containing Moran's I value and its significance (p-value).
            Prints detailed interpretation to console.
        """
        if data_column not in self.df.columns:
            raise ValueError(f"Column '{data_column}' not found in the dataframe.")

        # Ensure data is truly numeric and clean
        data_values = pd.to_numeric(self.df[data_column]).values

        # Calculate Moran's I using esda (using simulated p-value for robust inference)
        np.random.seed(42)  # Set seed for reproducibility of permutations
        mi = Moran(data_values, self.weights, permutations=999)

        # Interpretation logic
        interpretation = "RANDOM distribution."
        significance = "Not statistically significant."

        if mi.p_sim <= p_value_threshold:
            significance = f"Statistically significant (p-value: {mi.p_sim:.4f})."
            if mi.I > mi.EI_sim:
                interpretation = "POSITIVELY AUTOCORRELATED (Clustered)."
            elif mi.I < mi.EI_sim:
                interpretation = "NEGATIVELY AUTOCORRELATED (Dispersed/Checkboard)."

        # Detailed print output to console
        print("\n" + "=" * 60)
        print(f"Global Moran's I Analysis for variable: '{data_column}'")
        print("=" * 60)
        print(f"{'Statistic':<25} | {'Value':<15}")
        print("-" * 45)
        print(f"{'Moran I Observed (I)':<25} | {mi.I:^15.4f}")
        print(f"{'Moran I Expected (E[I])':<25} | {mi.EI_sim:^15.4f}")
        print(f"{'Z-score (based on sim)':<25} | {mi.z_sim:^15.4f}")
        print(f"{'p-value (simulated)':<25} | {mi.p_sim:^15.4f}")
        print("-" * 45)
        print(f"Significance: {significance}")
        print(f"Result: {interpretation}")
        print("=" * 60 + "\n")

        return mi.I, mi.p_sim

    def create_spatial_scatter(
        self, size_col, color_col, title="Spatial Distribution of Seville Parishes"
    ):
        """
        Task 5: Create a scatterplot with Lat/Lon on axes.
        - size_col: Determines point size.
        - color_col: Determines Red-Green color scale.
        """
        plt.figure(figsize=(12, 8))

        # Ensure numeric data
        sizes = pd.to_numeric(self.df[size_col])
        colors = pd.to_numeric(self.df[color_col])

        # Normalize size for visibility (scaling factor)
        # Using a square root scale often helps visualize area differences better
        display_sizes = (sizes / sizes.max()) * 1000

        # Create scatter plot
        # cmap='RdYlGn' provides the requested Red-Yellow-Green scale
        scatter = plt.scatter(
            self.df["Lon_Center"],
            self.df["Lat_Center"],
            s=display_sizes,
            c=colors,
            cmap="RdYlGn_r",
            alpha=0.7,
            edgecolors="black",
        )

        # Add labels for each parish
        for i, txt in enumerate(self.df["Parroquia"]):
            plt.annotate(
                txt,
                (self.df["Lon_Center"].iloc[i], self.df["Lat_Center"].iloc[i]),
                fontsize=8,
                alpha=0.8,
                xytext=(5, 5),
                textcoords="offset points",
            )

        # Formatting
        plt.colorbar(scatter, label=color_col)
        plt.xlabel("Longitude")
        plt.ylabel("Latitude")
        plt.title(title)
        plt.grid(True, linestyle="--", alpha=0.6)

        plt.show()

    def create_flexible_scatter(
        self,
        x_col,
        y_col,
        size_col=None,
        color_col=None,
    ):
        """
        Creates a scatter plot with optional size and color mapping.
        The color scale is inverted: Maximum = Red, Minimum = Green.
        """
        plt.figure(figsize=(12, 8))

        # Initialize default plot parameters
        plot_kwargs = {"alpha": 0.7, "edgecolors": "black"}

        # 1. Handle Size Variable
        if size_col and size_col in self.df.columns:
            # Normalize size to a visible range (e.g., 20 to 1000)
            sizes = pd.to_numeric(self.df[size_col], errors="coerce").fillna(0)
            plot_kwargs["s"] = (sizes / sizes.max()) * 1000 + 20
        else:
            plot_kwargs["s"] = 100  # Default size if None

        # 2. Handle Color Variable (Inverted Scale)
        if color_col and color_col in self.df.columns:
            plot_kwargs["c"] = pd.to_numeric(
                self.df[color_col], errors="coerce"
            ).fillna(0)
            # 'RdYlGn_r' makes Max=Red and Min=Green
            plot_kwargs["cmap"] = "RdYlGn_r"
        else:
            plot_kwargs["c"] = "steelblue"  # Default color if None

        # 3. Generate Plot
        scatter = plt.scatter(self.df[x_col], self.df[y_col], **plot_kwargs)

        # Add Colorbar if color is used
        if color_col:
            plt.colorbar(scatter, label=f"{color_col} (Red=High, Green=Low)")

        # 4. Final Formatting
        plt.title(
            "Relación entre la densidad de los corrales y del resto de viviendas en cada parroquia",
            fontsize=14,
        )
        plt.suptitle("densidad = población / nº viviendas, tamaño=población total")
        plt.xlabel("Densidad del resto de viviendas")
        plt.ylabel("Densidad de los corrales")
        slope = 4
        plt.axline((0, 0), slope=slope, color="black", linestyle="--")
        # label
        # angle = np.rad2deg(np.arctan(slope))

        plt.grid(True, linestyle="--", alpha=0.5)
        x_pos = 9  # manual
        y_pos = 30  # manual
        plt.text(
            x_pos,
            y_pos,
            "Línea de proporción x4",
            # rotation=angle,
            # rotation_mode="anchor",
            verticalalignment="top",  # Keeps it "above" the line
            fontsize=10,
        )
        # Optional: Annotate parish names if they exist in the dataframe
        if "Parroquia" in self.df.columns:
            for i, txt in enumerate(self.df["Parroquia"]):
                plt.annotate(
                    txt,
                    (self.df[x_col].iloc[i], self.df[y_col].iloc[i]),
                    fontsize=8,
                    xytext=(5, 5),
                    textcoords="offset points",
                )

        plt.show()


# =========================================================
# === MAIN EXECUTION AND MAPPING FUNCTION ===
# =========================================================
def run_analysis_and_mapping():
    # --- TASK 1/2: DATA LOADING AND CLEANING ---
    # Load raw text directly from the CSV string defined above
    try:
        if os.path.exists(DIR_CSV_DATA):
            df = pd.read_csv(DIR_CSV_DATA)
        else:
            df = pd.read_csv(io.StringIO(raw_csv_data))
            df.to_csv(DIR_CSV_DATA)
    except Exception as e:
        print(f"Error reading CSV data: {e}")
        return

    print("Data successfully loaded from CSV structure.")

    # Cleanup numeric formats: Remove thousand separators (.), replace decimal commas (,) with dots (.)
    # Extract number from "Corrales y su %...": e.g., "69-5,77" -> 69.0
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
    # Extract number from "Vecinos_Corrales...": e.g., "5.230-29,94" -> 5230.0
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

    # Note: 'Lat_Center' and 'Lon_Center' are already numerical floats from the input.

    # Remove total/aggregate rows for spatial calculation
    df_parishes = df.dropna(subset=["Lat_Center", "Lon_Center"]).copy()

    # --- TASK 3: CREATE COLOR MAP (Similar style to provided image) ---
    print("\nGenerating interactive colored map...")

    # Base map focused on historical Seville center
    seville_map = Map(location=[37.389, -5.992], zoom_start=15, tiles="OpenStreetMap")

    # Define color map based on population quantiles, matching a yellow-to-brown range
    # (A linear gradient of 'YlOrBr' is often a good visual approximation)
    color_scale = linear.YlOrBr_05.scale(
        df_parishes["Población"].min(), df_parishes["Población"].max()
    )
    color_scale.caption = "Población Total por Parroquia (XIX C.)"
    color_scale.add_to(seville_map)

    # Add each parish as a colored point to the map
    for _, row in df_parishes.iterrows():
        # Get color based on population
        point_color = color_scale(row["Población"])

        # Tooltip text for detail
        popup_text = (
            f"<b>Parroquia:</b> {row['Parroquia']}<br>"
            f"<b>Población:</b> {row['Población']}<br>"
            f"<b>Casas:</b> {row['Casas']}<br>"
            f"<b>Corrales (N-%_total):</b> {row['Corrales_Count']}-{row['Corrales_Pct']}<br>"
            f"<b>Población en corrales:</b> {row['Vecinos_Corrales_Count']}-{row['Vecinos_Corrales_Pct']}"
        )

        # Add CircleMarker (colored center points to approximate spatial clusters)
        CircleMarker(
            location=[row["Lat_Center"], row["Lon_Center"]],
            radius=10,  # Adjusted radius for visual density
            color="black",  # Border
            weight=1,
            fill=True,
            fill_color=point_color,
            fill_opacity=0.9,
            popup=Popup(popup_text, max_width=300),
            tooltip=f"{row['Parroquia']}: {row['Población']}",
        ).add_to(seville_map)

    # Save map to HTML file
    map_filename = "seville_parish_population_map.html"
    seville_map.save(map_filename)
    print(f"Map saved successfully as '{map_filename}'.")

    # Optionally attempt to open in the browser automatically (works on most platforms)
    try:
        webbrowser.open("file://" + os.path.realpath(map_filename))
    except Exception:
        print(
            f"Map created but could not open in browser. Please open '{map_filename}' manually."
        )

    # --- TASK 4: SPATIAL AUTO-CORRELATION ANALYSIS ---
    # Initialize our calculator class
    sac_calc = SpatialAutoCorrCalculator(df_parishes)

    # Calculate Moran's I for Pct variables of df
    print("#" * 10 + "Neighbours density" + "#" * 10)
    sac_calc.get_morans_i("Vecinos_Corrales_Pct")  # RANDOM
    print("#" * 10 + "Households density" + "#" * 10)
    sac_calc.get_morans_i("Corrales_Pct")  # THIS NOT RANDOM BUT COUNT IT IS
    # sac_calc.get_morans_i("Población") --> NOT RANDOM
    # sac_calc.get_morans_i("Casas") --> RANDOM
    # np.corrcoef(df["Corrales_Pct"], df["Vecinos_Corrales_Pct"]) 0.78
    # np.corrcoef(df["Corrales_Pct"], df["Casas"]) -0.14
    # np.corrcoef(df["Población"], df["Vecinos_Corrales_Pct"]) -0.22
    # np.corrcoef(df["Población"], df["Casas"]) 0.25
    # np.corrcoef(df["Households_Density"], df["Corrales_Density"]) # -0.15

    # --- Execution Example to add in run_analysis_and_mapping() --
    # # ADD Heatmap w/ all vars & scatter of household vs corrales density with 45º line-
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
    sac_calc.create_spatial_scatter(
        size_col="Población",
        color_col="Households_Density",
        title="Seville Households Density in the late XIX Century (est.)",
    )
    sac_calc.create_spatial_scatter(
        size_col="Población",
        color_col="Corrales_Density",
        title="Seville Households Density in the late XIX Century (est.)",
    )
    sac_calc.create_flexible_scatter(
        "Households_Density", "Corrales_Density", "Población"
    )

    # 1. Initialize the raw data
    data = {
        1799: [80598, "González de León"],
        1821: [75000, "Censo del Trienio"],
        1832: [96683, "Guía de Herrera Dávila"],
        1857: [112529, "dummy", 34.89], # Censo Estatal
        1860: [118298, "Censo Estatal"],
        1877: [134318, "Censo Estatal", 36.78],
        1887: [143182, "Censo Estatal"],
        1897: [145728, "Censo Estatal"],
        1900: [148315, "Censo Estatal", 37.42],
        1910: [158287, "Censo Estatal"],
        1920: [205529, "Censo Estatal"],
    }

    # Convertir a DataFrame manejando la longitud variable de las listas
    rows = []
    for year, vals in data.items():
        pop = vals[0]
        source = vals[1]
        pct_u20 = vals[2] if len(vals) > 2 else None
        rows.append({'Year': year, 'Population': pop, 'Source': source, 'Pct_Under20': pct_u20})

    df = pd.DataFrame(rows)

    # Calcular población absoluta menor de 20 años
    df['Pop_Under20'] = (df['Population'] * df['Pct_Under20'] / 100)
    # 3. Add "Source Type" logic
    def categorize_source(row):
        if "Censo Estatal" not in row["Source"]:
            return "pre-Censo Estatal"
        elif 1800 <= row["Year"] <= 1901:
            return "Censo Estatal (S. XIX)"
        else:
            return "Censo Estatal (S. XX)"

    df["Source Type"] = df.apply(categorize_source, axis=1)

    # --- Visualization ---
    fig, ax = plt.subplots(figsize=(14, 7))

    # 1. Plot Segmented Population Line
    color_map = {
            "pre-Censo Estatal": "gray",
            "Censo Estatal (S. XIX)": "tab:red",
            "Censo Estatal (S. XX)": "tab:green"
        }
    for i in range(len(df) - 1):
        x_seg, y_seg = df['Year'].iloc[i:i+2], df['Population'].iloc[i:i+2]
        ax.plot(x_seg, y_seg, color=color_map[df['Source Type'].iloc[i+1]], lw=2.5, marker='o', markersize=5)

    # B. Línea de Población < 20 años (Discontinua, solo donde hay datos)
    df_u20 = df.dropna(subset=['Pop_Under20'])
    ax.plot(df_u20['Year'], df_u20['Pop_Under20'], color='orange', linestyle='--', 
            lw=2, marker='s', markersize=7, label='Población < 20 años (Calculada)')

    # Añadir etiquetas de porcentaje sobre los puntos naranja
    for i, row in df_u20.iterrows():
        ax.annotate(f"{row['Pct_Under20']}%", (row['Year'], row['Pop_Under20']), 
                    textcoords="offset points", xytext=(0,10), ha='center', 
                    color='darkorange', fontweight='bold', fontsize=9)
    # 2. Add Vertical Division Lines
    boundary_years = [1814, 1837, 1873]
    for year in boundary_years:
        ax.axvline(x=year, color='black', linestyle='--', alpha=0.4)

    # C. Divisiones Históricas y Anotaciones
    boundary_years = [1814, 1837, 1873]
    for year in boundary_years:
        ax.axvline(x=year, color='black', linestyle=':', alpha=0.3)

    periods = [
        (1799, 1814, "Carlos IV y\nGuerra Indep."),
        (1814, 1837, "Fernando VII"),
        (1837, 1873, "Isabel II"),
        (1873, 1920, "Restauración")
    ]

    y_top = ax.get_ylim()[1]
    for start, end, label in periods:
        mid = start + (end - start) / 2
        ax.text(mid, y_top * 0.93, label, ha='center', fontsize=9, 
                style='italic', bbox=dict(facecolor='white', alpha=0.6, edgecolor='none'))

    # --- 3. Formato Final ---
    ax.set_title("Evolución de la Población Total y de jóvenes en Sevilla", fontsize=16, pad=25)
    ax.set_ylabel("Número de Habitantes")
    ax.set_xlabel("Año")
    ax.grid(True, axis='y', linestyle='--', alpha=0.5)

    # Leyenda personalizada
    legend_elements = [
        Line2D([0], [0], color='gray', lw=3, label='Fuente: Historiadores (pre-Censo)'),
        Line2D([0], [0], color='tab:blue', lw=3, label='Fuente: Censo Estatal S. XIX'),
        Line2D([0], [0], color='tab:red', lw=3, label='Fuente: Censo Estatal S. XX'),
        Line2D([0], [0], color='orange', lw=2, linestyle='--', marker='s', label='Población < 20 años')
    ]
    ax.legend(handles=legend_elements, loc='lower right', title="Categorías de Datos")

    plt.tight_layout()
    plt.show()


if __name__ == "__main__":
    run_analysis_and_mapping()
