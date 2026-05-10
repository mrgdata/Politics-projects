import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


# 1. Definición de datos
dict_xvi = {
    "Censos": 46,
    "Deudas": 19.6,
    "Bienes Muebles": 9.8,
    "Fincas urbanas": 13.1,
    "Fincas rústicas": 7.9,
    "Otros": 3.6,
}

dict_xvii = {
    "Censos": 34,
    "Deudas": 23.6,
    "Bienes Muebles": 24.2,
    "Fincas urbanas": 8.43,
    "Fincas rústicas": 6.77,
    "Otros": 3,
}

# 2. Creación del DataFrame comparativo
df_comp = pd.DataFrame([dict_xvi, dict_xvii], index=["Siglo XVI", "Siglo XVII"]).T

# 3. Cálculo de la diferencia absoluta (Variación)
df_comp["Variación (%)"] = df_comp["Siglo XVII"] - df_comp["Siglo XVI"]

# 4. Visualización
fig, ax = plt.subplots(figsize=(12, 7))
df_comp[["Siglo XVI", "Siglo XVII"]].plot(
    kind="bar", ax=ax, color=["#df4f4f", "#44d444"], edgecolor="black"
)

# Formato y anotaciones
ax.set_title(
    "Evolución de la Estructura Patrimonial (Inventarios Post-Mortem)",
    fontsize=14,
    pad=20,
)
ax.set_ylabel("Porcentaje sobre el Total (%)")
ax.set_xlabel("Categorías de Bienes")
ax.grid(axis="y", linestyle="--", alpha=0.6)

# Añadir etiquetas de valor sobre las barras
for p in ax.patches:
    ax.annotate(
        f"{p.get_height()}%",
        (p.get_x() + p.get_width() / 2.0, p.get_height()),
        ha="center",
        va="center",
        xytext=(0, 9),
        textcoords="offset points",
        fontsize=9,
    )

plt.xticks(rotation=45)
plt.tight_layout()
plt.show()

# Mostrar tabla de comparación
print("Tabla Comparativa de Inventarios:")
print(df_comp.sort_values(by="Variación (%)", ascending=False))

# Datos extraídos de la imagen
categories = [
    "0/250.000",
    "250 / 500.000",
    "500.000 / 1 Millón",
    "1 a 3 Mills.",
    "3 a 6 Mills.",
    "6 a 10 Mills.",
    "10 a 20 Mills.",
    "20 a 30 Mills.",
    "Más de 30 Mills.",
    "Total",
]

muebles = [81.23, 72.64, 63.47, 40.75, 31.50, 37.02, 21.10, 19.55, 33.06, 32.58]
inmuebles = [18.77, 27.36, 36.53, 59.25, 68.50, 62.98, 78.90, 80.45, 66.94, 67.42]

# Invertimos el orden para que coincida con la disposición visual (0/250.000 arriba)
categories.reverse()
muebles.reverse()
inmuebles.reverse()

# Configuración del gráfico
fig, ax = plt.subplots(figsize=(10, 8))

# Colores similares a la escala de grises de la imagen
color_muebles = "#df4f4f"  # Gris claro ["#df4f4f", "#d8af3e"]
color_inmuebles = "#44d444"  # Gris oscuro

# Creación de las barras apiladas
bar_width = 0.6
indices = np.arange(len(categories))

# El "Total" suele tener un pequeño espacio extra en este tipo de gráficos
# Aquí simplemente lo graficamos, pero podrías ajustar los índices si quisieras un gap
bars1 = ax.barh(
    indices,
    muebles,
    bar_width,
    label="Muebles",
    color=color_muebles,
    edgecolor="black",
    linewidth=0.5,
)
bars2 = ax.barh(
    indices,
    inmuebles,
    bar_width,
    left=muebles,
    label="Inmuebles",
    color=color_inmuebles,
    edgecolor="black",
    linewidth=0.5,
)

# Añadir etiquetas de texto con los porcentajes dentro de las barras
for i, (m, inm) in enumerate(zip(muebles, inmuebles)):
    # Etiqueta Muebles
    ax.text(
        m / 2,
        i,
        f"{m:.2f}%",
        ha="center",
        va="center",
        color="black",
        fontsize=9,
        fontweight="bold",
    )
    # Etiqueta Inmuebles
    ax.text(
        m + inm / 2,
        i,
        f"{inm:.2f}%",
        ha="center",
        va="center",
        color="black",
        fontsize=9,
        fontweight="bold",
    )

# Estética y formato
ax.set_title(
    "Distribución de bienes muebles e inmuebles en el patrimonio post-morten",
    pad=20,
    fontsize=12,
    fontweight="bold",
)
plt.suptitle(
    "Sevilla, Siglo XVI (en maravedíes). Inmuebles: Censos, juros y fincas rústicas y urbanas"
)
ax.set_yticks(indices)
ax.set_yticklabels(categories, fontsize=10)
ax.set_xticks([0, 25, 50, 75, 100])
ax.set_xticklabels(["0,00%", "25,00%", "50,00%", "75,00%", "100,00%"], fontsize=10)

# Cuadrícula vertical (solo en el eje X)
ax.grid(axis="x", linestyle="-", alpha=0.3)
ax.set_axisbelow(True)

# Leyenda en la parte inferior
ax.legend(loc="upper center", bbox_to_anchor=(0.7, -0.05), ncol=2, frameon=False)

# Etiquetas de pie de gráfico
plt.figtext(0.05, 0.05, "Total de los bienes", ha="left", fontsize=10)

plt.tight_layout()
plt.show()


###############################
# 1. Definición de datos
vecinos = {
    "1882 (est.)": 463,
    "1991": 64,
    "2001": 28,
}
corrales = {
    "1882 (est.)": 734,
    "1991": 422,
    "2001": 208,
}

# 2. Creación del DataFrame comparativo
df_comp = pd.DataFrame(
    [vecinos, corrales], index=["Vecinos (en cientos)", "Corrales"]
).T

# 4. Visualización
fig, ax = plt.subplots(figsize=(12, 7))
df_comp[["Vecinos (en cientos)", "Corrales"]].plot(
    kind="bar", ax=ax, color=["#df4f4f", "#44d444"], edgecolor="black"
)

# Formato y anotaciones
ax.set_title(
    "Evolución del número de corrales y sus habitantes",
    fontsize=14,
    pad=20,
)
ax.set_ylabel("Total")
ax.grid(axis="y", linestyle="--", alpha=0.6)

# Añadir etiquetas de valor sobre las barras
for p in ax.patches:
    ax.annotate(
        f"{p.get_height()}",
        (p.get_x() + p.get_width() / 2.0, p.get_height()),
        ha="center",
        va="center",
        xytext=(0, 9),
        textcoords="offset points",
        fontsize=9,
    )

plt.xticks(rotation=45)
plt.tight_layout()
plt.show()
