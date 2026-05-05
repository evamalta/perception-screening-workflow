# Analytical workflow: Screening stakeholder perceptions of biological invasions to inform local governance

This repository provides the analytical workflow used in the manuscript:  
**Screening stakeholder perceptions of biological invasions to inform local governance: insights from a multi-sectoral community in Northern Portugal**

---

## 📊 Overview

This project analyses multi-sectoral stakeholder perceptions of biological invasions using a multi-dimensional framework combining:

- Problem framing
- Species of concern  
- Management acceptability  
- Expectations for collaboration  
- Local spatial knowledge 

The analyses aim to identify areas of consensus, complementarity, and divergence across stakeholder sectors to inform the design of arrangements for the local governance of biological invasions.

---

## 🗂️ Repository structure

- `data/` – structured datasets used to run the workflow (raw original data is not publicly available due to ethical and data protection constraints; the datasets provided here are generated to reproduce the structure and enable the workflow to run)
- `scripts/` – R scripts for each analytical component

---

## 🧩 Data structure

Due to ethical considerations and data protection requirements, the original participant-level datasets cannot be publicly shared.

To enable reproducibility of the analytical workflow, the datasets provided in this repository reproduce the structure and variables of the original data.

### words.xlsx
- `participant_id`  
- `stakeholder_sector`  
- `word`  
- `word_eng` (optional; used for visualisation)  

### species.xlsx
- `participant_id`  
- `stakeholder_sector`  
- `species`  
- `species_group`  
- `in_EU_list` (yes/no)  
- `in_PT_list` (yes/no)  

### comfort.xlsx
- `participant_id`  
- `stakeholder_sector`  
- `plants`  
- `mammals`  
- `birds`  
- `herpetofauna`  
- `insects`  
- `other_invertebrates`  

### expectations_agreement.xlsx
- `participant_id`  
- `stakeholder_sector`  
- `expectation_nr`  
- `expectation`  
- `coder1`  
- `coder2`  

### expectations.xlsx
- `participant_id`  
- `stakeholder_sector`  
- `expectation_nr`  
- `expectation_category`  

### spatial_data.shp
- `id`  
- `stake_sec` (stakeholder sector)  
- `map_dim` (Occurrence, Impacts, Management, Detection)  

### study_area.shp
- study area boundary polygon  

All scripts assume that these files are located in the `data/` directory and follow the structure described above.

---

## 🔐 Data availability

Raw participant-level data cannot be made publicly available due to ethical and privacy constraints.

Aggregated datasets supporting the results are available via an external repository, as described in the manuscript.

---

## ✅ Requirements

The analysis was conducted in R. The packages used include:

- tidyverse  
- readxl  
- writexl  
- openxlsx  
- janitor  
- vegan  
- igraph  
- ggraph  
- ggrepel  
- rstatix  
- irr  
- sf  
- MASS  
- raster  
- corrplot  

---

## 📝 Additional notes

Some preprocessing steps were performed manually prior to analysis, including:

- harmonisation of framing words  
- standardisation of species names  
- classification of species into taxonomic groups and alignment with EU and national policy lists  
- thematic classification of expectation statements  
- digitisation and attribution of participatory mapping data in QGIS  

---

## ▶️ Running the analysis

To reproduce the full workflow, run:

```r
source("scripts/run_all.R")