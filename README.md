# Contemporary Trajectories and Driving Factors of Amazon Forest Degradation

Official code repository supporting the analytical workflows and statistical modeling for the manuscript:

> **Contemporary trajectories and driving factors of Amazon forest degradation**  
> *Status: Under Review*  
> **Authors:** Argemiro Teixeira Leite Filho et al. (National Institute for Space Research - INPE)

---

## Abstract & Key Findings

While international conservation policies prioritize clear-cut deforestation, tropical forests are increasingly degraded by fires, edge effects, timber extraction, and extreme droughts. 

This repository implements the spatial and statistical analyses characterizing contemporary forest degradation trajectories in the Brazilian Amazon between **2008 and 2024**:

- **Spatial Extent:** Forest degradation affected an area **20% larger than clear-cut loss**, with annual degradation rates exceeding clear-cut rates in **82% of the analyzed years**.
- **Persistence & Coupling:** While **84% of degradation persists autonomously**, the coupling between degradation and deforestation is intensifying (**18.5% of total clear-cut deforestation is preceded by degradation**).
- **Driving Factors:** Statistical modeling explains **27% of spatial variability** in degradation through the synergistic interactions of **cumulative dry days**, **lagged forest clearing**, and **tropical timber market prices**.

---

## Analytical Scope

The codebase in this repository is structured to execute:

1. **Spatial Trajectory Processing (2008–2024):** Quantifying spatial extent, annual degradation rates, autonomous persistence, and degradation-preceded deforestation coupling.
2. **Driver Factor Integration & Modeling:** Data integration and statistical evaluation combining climate indicators (cumulative dry days), land-use history (lagged clearing), and economic variables (timber price proxy).

---

## Requirements

The analysis relies on Python 3.8+ with standard scientific and geospatial computing libraries:

```bash
pip install numpy pandas scipy statsmodels geopandas rasterio matplotlib seaborn
