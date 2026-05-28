# My Portfolio

A collection of my data engineering and data science projects, showcasing my ability to solve problems across multiple programming languages and ecosystems.

## 📁 Repository Structure

*   **`macros.sas`** — A library of production-grade SAS macros built to automate single factor analysis, variable transformation, optimal binning, and modelling process.
*   **`titanic.py`** — A simple illustration for a complete end-to-end machine learning pipeline in Python using the Titanic dataset to predict passenger survival through exploratory data analysis and various ML algorithms.
*   **`README.md`** — This documentation file outlining the repository overview.

---

## 🏗️ File Breakdown

### 1. SAS Utility Macros (`macros.sas`)
This script demonstrates my proficiency in **Base SAS** and advanced **SAS Macro Programming**. It focuses on backend automation and building reusable, robust data engineering components.
*   **Macros:** variable screening | box-cox transformation | WOE/IV calculation | correlation analysis | variable clustering using VARCLUS | optimal binning base on entropy (supervised) | level clustering of categorical inputs | computation of c-statistics for both training and validation datasets | 3-Step approach in model selection as proposed by Shtatland, Kleinman, and Cain (2003): Stepwise Methods in Using SAS PROC LOGISTIC and SAS Enterprise Miner for Prediction | model selection based on the purposeful selection algorithm as proposed by Bursac, Gauss, Williams, Kleinman, and Hosmer (2008): Purposeful	Selection of Variables in Logistic Regression.

### 2. Python Predictive Modeling on binary outcome (`titanic.py`)
This script illustrate the data science workflow in **Python**, utilizing modern open-source libraries to clean data, extract features, and train predictive models.
*   **Tech Stack:** `pandas`, `numpy`, `scikit-learn`, `matplotlib` / `seaborn`.
*   **Core Concepts:** Handling missing data, categorical encoding, feature engineering, model selection, hyperparameter tuning.

---

## ⚠️ Disclaimer

The code provided in this repository is shared strictly for educational and portfolio demonstration purposes. 

*   **Environment Specific:** These scripts were built and fully validated within my specific local development environments.
*   **No Warranty:** They are provided "as-is" without any warranties of any kind. 
*   **Run at Your Own Risk:** The author assumes no responsibility for any system errors, data loss, or unintended behavior caused by running this code on your own machine or servers. Always review the scripts thoroughly before execution.

---

## 🚀 Getting Started

### Prerequisites
*   To run the SAS script: Access to **SAS 9.4** or **SAS Viya**.
*   To run the Python script: **Python 3.8+** with the following packages installed:
    ```bash
    pip install pandas numpy scikit-learn matplotlib seaborn
    ```
*   *   Download the Titanic data files from [Kaggle](https://www.kaggle.com/c/titanic/data), and change the python script to point to your file.

### Execution
*   **SAS:** Open `macros.sas` in your SAS environment and submit the code to compile the macros.
*   **Python:** Execute the script via your terminal:
    ```bash
    python titanic.py
    ```
