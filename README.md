# POST-HOC VALIDATION AND PREDICTION WORKFLOW OF HUNTING BEHAVIOUR DERIVED FROM UNSUPERVISED CLASSIFICATION: A CASE STUDY OF ADÉLIE PENGUINS

**Note:** this workflow assumes that accelerometry data is already classified (we used Expectation Maximisation)
<br /><br />
Further information on the EM model used in our case study can be found in the online repository from Chimienti et al. 2022 (https://github.com/MariannaChimi/MuFFIN_MSCA.git).

## WORKFLOW CODE SCRIPTS

### SCRIPT 1: Saving PCE annotations.R

* A simple tool for saving manual annotations from the output files of VANTAGE.
* Particularly useful when breaking up annotation sessions across different videos or time periods.

**Workflow:**
**EM-Classified Accelerometry Data** → **VANTAGE** → **EM-Classified Accelerometry OUT File** → **Script 1 (R)** → **PCE Annotation File + Reset OUT File**

---

### SCRIPT 2: PCE Analysis.ipynb

* Compiles all PCE Annotation Files for each individual and attaches them to the corresponding EM-Classified Accelerometry Data.
* Clips EM-Classified Accelerometry Data to periods covered by video.
* Adds `Timestampvideo` column to account for discrepancy between logger and video timestamps (allows quick reference from logger data to video frames).
* Adds `hunting_cluster` (EM hunting window) and `Dive ID` columns for each individual.
* Adds `window_size` (EM hunting window duration) column for each individual.
* Calculates appropriate temporal buffer size using the distances between non-classified PCE annotations and their closest EM hunting windows.
* Saves formatted files for modelling.
* **Extra:** Includes preliminary plots for exploring data and classification performance.

**Workflow:**
**EM-Classified Accelerometry Data + PCE Annotation File(s)** → **Script 2 (Jupyter Notebook)** → **EM-Classified Accelerometry Data with PCE**

---

### SCRIPT 3: Model datasets.R

* Stacks together the EM-classified accelerometry data (with attached PCE) for each individual.
* Removes dives flagged as low visibility.
* Buffers unvalidated PCE labels onto the nearest EM hunting window within the calculated distance threshold, and builds a matrix of prey type/count per window.
* Adds dive duration, hunting window duration, and max dive depth columns.
* Builds five model-ready datasets: presence/absence and prey intensity, each at window level and dive level, plus a simplified window-level intensity dataset.
* Saves formatted datasets for modelling.

**Workflow:**
**EM-Classified Accelerometry Data with PCE** → **Script 3 (R)** → **Model-ready datasets (TF1, TF2, I1, I1.1, I2)**

---

### SCRIPT 4: Modelling.R

* Loads the five model-ready datasets produced by Script 3.
* Removes individuals with low quality data.
* Fits GLMMs testing prey presence/absence and prey intensity against hunting window size, dive metrics, VeDBA energetics, and prey type.
* Runs automated model selection across predictor combinations, comparing candidate models by AIC.
* Checks model diagnostics (simulated residuals, collinearity, ROC/AUC) and produces prediction plots for each model.

**Workflow:**
**Model-ready datasets** → **Script 4 (R)** → **Fitted models and diagnostics**

