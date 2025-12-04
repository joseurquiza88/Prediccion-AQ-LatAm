# 📊 Predicción de concentraciones de PM2.5 en Áreas Urbanas de América Latina

Este repositorio contiene scripts, notebooks y recursos para el **análisis y prediccion de la contaminacion atmosferica (PM2.5)** en centros urbanos de América Latina, usando **diversas variables predictivas** y comparando múltiples enfoques de modelado.

---

## 🎯 Objetivo

- Desarrollar **modelos predictivos** para estimar concentraciones de PM2.5 a nivel **espacial y temporal** para distintos centros ubranos de America Latina.  
- Evaluar y comparar distintos enfoques de **machine learning y modelos estadísticos**, incluyendo:
  - Regresion Lineal Simple (PM2.5-AOD)
  - Regresion Lineal Multiple
  - Correccion del AOD por variables atmosfericas
  - Ridge - LASSO
  - Modelo Lineal Generalizado (GLM)
  - Modelo lineal mixto (LME)
  - Modelo aditivo generalizado (GAM)
  - Support Vector Regression (SVR) 
  - Random Forest (RF)
  - Extremely Randomized Trees (ET) 
  - Extreme Gradient Boosting (XGB)  

---

## 🛠 Este repositorio incluye

- **Metodologia y variables predictivas:**
La carpeta [`/00_Informacion_de_Base`](./00_Informacion_de_Base/) reúne la metodología aplicada en cada etapa del proyecto, junto con la descripción de las variables predictivas principales utilizadas en los modelos.

- **Procesamiento de datos:**  
  Limpieza, interpolación y normalización de variables satelitales y de superficie (AOD, meteorología, composición de aerosoles, otras).  

- **Modelado comparativo:**  
  Entrenamiento y evaluación de múltiples modelos predictivos para identificar cuáles capturan mejor la variabilidad espacial y temporal de PM2.5.  

- **Análisis de desempeño:**  
  Cálculo de métricas como R², RMSE, Pearson y Bias, y generación de gráficos y mapas de predicción.  

- **Reproducibilidad:**  
  Scripts y notebooks organizados para permitir la replicación completa de los análisis y la generación de resultados.
  
- **Ejemplos de uso** (🚧 En desarrollo)
  La carpeta [`/06_Codigos ejemplo`](./06_Codigos_ejemplo/) contiene scripts en Python y R que sirven como guía para cargar, visualizar y procesar las imágenes raster y los archivos asociados al proyecto.

---

## 📦 Bases de datos disponibles

Predicciones de las concentraciones de PM₂.₅ para cinco ciudades de América Latina (2015–2024).
- 🔹 LATAM_PM2.5_1km_Anual: https://doi.org/10.5281/zenodo.17792065
- 🔹 LATAM_PM2.5_1km_Mensual (2024): https://doi.org/10.5281/zenodo.17794205
- 🔹 LATAM_PM2.5_1km_Mensual (2023): https://doi.org/10.5281/zenodo.17801932
- 🔹 LATAM_PM2.5_1km_Mensual (2022): https://doi.org/10.5281/zenodo.17802809
- 🔹 LATAM_PM2.5_1km_Mensual (2021): https://doi.org/10.5281/zenodo.17802946
- 🔹 LATAM_PM2.5_1km_Mensual (2020): https://doi.org/10.5281/zenodo.17803276
- 🔹 LATAM_PM2.5_1km_Mensual (2019): https://doi.org/10.5281/zenodo.17803439
- 🔹 LATAM_PM2.5_1km_Mensual (2018): https://doi.org/10.5281/zenodo.17804032
- 🔹 LATAM_PM2.5_1km_Mensual (2017): https://doi.org/10.5281/zenodo.17804354
- 🔹 LATAM_PM2.5_1km_Mensual (2016): https://doi.org/10.5281/zenodo.17804477
- 🔹 LATAM_PM2.5_1km_Mensual (2015): https://doi.org/10.5281/zenodo.17804648
  

## 🗺️ Mapas de concentraciones de PM₂.₅ para cinco ciudades de América Latina (2024).


<img width="800" height="600" alt="Concentraciones Anuales por ciudad_2024" src="https://github.com/user-attachments/assets/fba89c68-9a08-4084-98c7-a263c9ae6fec" />

---

Esta investigación se desarrolla en el marco de una tesis de doctorado en Ingenieria Ambiental de la Universidad Tecnologica Nacional -  Facultad Regional Mendoza actualmente en proceso 🚧
