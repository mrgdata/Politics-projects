# Proyectos de Ciencia Política

Repositorio con varios proyectos de investigación en comportamiento político, análisis electoral y políticas públicas. Cada proyecto está en su propia carpeta con scripts numerados por orden de ejecución.

## Estructura del repositorio

```
Politics-projects/
│
├── class-voting-spain/            # Voto de clase en España
│   ├── data/                      # Datos CIS (.sav)
│   ├── 01_data_prep.R             # Carga y recodificación
│   ├── 02_models.R                # Regresiones logísticas
│   └── 03_visualization.R         # Gráficos y tablas (+ ESS9)
│
├── political-participation-wvs/   # TFG: participación política (WVS7)
│   ├── 01_data_prep.R             # Merge WVS + índices, variables
│   ├── 02_eda_pca.R               # ACP y descriptivos
│   ├── 03_random_forest.R         # 4 bosques aleatorios (NA/LA)
│   ├── 04_logit_models.R          # Logit y modelos lineales
│   └── 05_visualization.R         # Variable importance y descriptivos
│
├── electoral-inequality-ess/      # Desigualdad electoral en Europa
│   ├── 01_data_prep.R             # Carga ESS9 + WID
│   └── 02_analysis_viz.R          # Indicadores P90/P50 y gráficos
│
├── did-electoral-trends/          # DiD tendencias electorales
│   ├── 01_data_prep.R             # Carga y variables de tratamiento
│   ├── 02_models.R                # Modelos DiD (felm) y ANOVA
│   └── 03_visualization.R         # Tendencias, boxplots, efectos
│
├── france-usa-electoral/          # Análisis electoral Francia y EEUU
│   ├── 01_usa_density.R           # Densidad vs Trump/Biden
│   ├── 02_france_aggregate.R      # Le Pen/Macron datos agregados
│   └── 03_france_ess_individual.R # ESS9: logit voto Le Pen
│
├── residential-mobility-abm/      # Modelo de segregación residencial
│   ├── renta_politica.nlogo       # Modelo ABM (NetLogo)
│   └── analysis.R                 # Análisis exploratorio resultados
│
├── seville-historical-segregation/ # Segregación en Sevilla (S.XIX)
│   ├── 01_data_prep.py            # Carga y limpieza de datos
│   ├── 02_spatial_analysis.py     # I de Moran, autocorrelación
│   ├── 03_mapping.py              # Mapa Folium interactivo
│   └── 04_visualization.py        # Scatter plots y población histórica
│
├── party-positioning-ess/         # Posicionamiento de partidos
│   └── Diagrama partidos.ipynb    # Notebook: ESS9 Alemania y España
│
├── urban-rehabilitation-isa/      # Políticas urbanas y movilidad
│   ├── ISA2023.Rmd                # Informe R Markdown (DiD + SEM)
│   └── ISA2023.html               # Informe renderizado
│
├── .gitignore
├── README.md
│
└── old/                           # Archivos originales (sin reorganizar)
    ├── scripts/r/                 # Scripts R originales
    ├── scripts/py/                # Scripts Python originales
    └── other/                     # Otros archivos originales
```

## Proyectos

### 1. Voto de clase en España (`class-voting-spain/`)
Regresiones logísticas para evaluar qué clases sociales votan a los diferentes bloques ideológicos en España (2011-2019), controlando por factores sociodemográficos. Incluye análisis de valores políticos con datos ESS9. **R**

### 2. Participación política en las Américas (`political-participation-wvs/`)
TFG. Bosques aleatorios y regresiones logísticas para predecir la participación política en América del Norte y Latinoamérica usando la World Values Survey Wave 7. Incluye ACP para agrupar variables. **R**

### 3. Desigualdad electoral en Europa (`electoral-inequality-ess/`)
Indicador de desigualdad P90/P50 basado en las proporciones de voto según renta, comparado con el indicador de desigualdad de ingresos (WID), agrupado por países europeos. **R**

### 4. Diferencias en diferencias electorales (`did-electoral-trends/`)
Modelos DiD con errores estándares agrupados y ANOVAs para evaluar el impacto de la reconversión industrial y la crisis económica en las tendencias electorales de Murcia y Sevilla. **R**

### 5. Análisis electoral Francia y EEUU (`france-usa-electoral/`)
Análisis descriptivos y regresión logística sobre los perfiles del electorado de Le Pen, Macron, Mélenchon y Trump/Biden. Datos agregados (WPID, INSEE, censos) e individuales (ESS9). **R**

### 6. Modelo de segregación residencial (`residential-mobility-abm/`)
Modelo de agentes tipo Schelling (NetLogo) que evalúa si es mejor una política de renta mínima o de renta universal para la satisfacción residencial. Incluye análisis exploratorio de los resultados en R. **NetLogo + R**

### 7. Segregación histórica en Sevilla (`seville-historical-segregation/`)
Análisis espacial de las parroquias de Sevilla en el siglo XIX: autocorrelación espacial (I de Moran), mapa interactivo Folium y evolución demográfica histórica. **Python**

### 8. Posicionamiento de partidos (`party-positioning-ess/`)
Visualización de las posiciones de los partidos políticos en Alemania y España según variables sociodemográficas y de opinión (ESS9). **Python (Jupyter)**

### 9. Políticas urbanas y movilidad residencial (`urban-rehabilitation-isa/`)
Análisis del efecto de las políticas de rehabilitación urbana de la UE en Andalucía sobre las preferencias de movilidad residencial. Usa DiD y Modelos de Ecuaciones Estructurales (SEM). **R Markdown**

## Notas

- **Datos locales**: la mayoría de los scripts cargan datos desde rutas locales. Si no se encuentran los archivos, se mostrará un mensaje indicando dónde colocarlos.
- **Datasets públicos**: los datos se pueden encontrar públicamente (CIS, ESS, WVS, WID, INSEE, etc.).
- Un análisis de los proyectos puede leerse en mi blog: https://medium.com/@mromgar99
