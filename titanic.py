dsn_mdl   = "data/titanic_train.csv"        # training dataset, including relative location
dsn_tst   = "data/titanic_test.csv"         # validation dataset, including relative location
out_num   = "output/titanic_num.csv"        # output file for numeric variables statistics
out_ctg   = "output/titanic_ctg.csv"        # output file for categorical variables frequency distribution
out_woe   = "output/titanic_woe.csv"        # output file for WoE and IV
out_img   = "output/titanic_"               # prefix for 2x2 contingency table heatmaps
var_resp  = "Survived"
var_num   = ["Age", "Fare", "SibSp", "Parch"]
var_ctg   = ["Pclass", "SibSp", "Parch", "Sex", "Embarked"]
drop_cols = ["Name", "Ticket", "Cabin"]

#-------------------------- IMPORT LIBRARIES ---------------------------#
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import math
from sklearn.metrics import classification_report, confusion_matrix
from sklearn.metrics import mean_squared_error, mean_absolute_error, accuracy_score
from itertools import combinations
from statsmodels.stats.outliers_influence import variance_inflation_factor
import statsmodels.api as sm
import statsmodels.formula.api as smf
from pandas.api.types import is_numeric_dtype
from sklearn.model_selection import cross_val_predict, StratifiedKFold
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline

# for models
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.svm import SVC
from sklearn.naive_bayes import GaussianNB
from xgboost import XGBClassifier
from sklearn.neural_network import MLPClassifier

#-------------------------- DEFINE FUNCTIONS ---------------------------#
def fn_num_hist(df, var, k_bin): # Generate histograms for numerical variables
    print(f"=== Creating histogram for {var} ===")
    bin_edges = np.linspace(df[var].min(), df[var].max(), num=k_bin+1)
    plt.figure(figsize=(k_bin, 5))
    ax = sns.histplot(data=df, x=var, bins=bin_edges, color="skyblue", edgecolor="black")
    for container in ax.containers:
        ax.bar_label(container)
    plt.title(f"Distribution of {var}", fontsize=14)
    plt.xlabel(f"{var} Group", fontsize=12)
    plt.ylabel("Count (Frequency)", fontsize=12)
    plt.tight_layout()
    img_filename = f"{out_img}histgram_{var}.png"
    plt.savefig(img_filename, dpi=300)
    plt.close()

def fn_opt_bin(df, var, resp): # Optimal binning using ML approach (Decision Trees)
    print(f"=== Optimal binning for {var} ===")
    clean_df = df[[var, resp]].dropna()

    tree = DecisionTreeClassifier(max_depth=3, min_samples_leaf=0.05)
        # max number of groups = 2^3, min group size is 5% of all observations
    tree.fit(clean_df[[var]], clean_df[resp])
    var_splits = np.sort(tree.tree_.threshold[tree.tree_.threshold != -2])
    print(f"Optimal Split Points for {var}:", var_splits)

    # create new variable based on the splits
    edges = [-np.inf] + list(var_splits) + [np.inf]
    labels = [f"{i + 1}. {edges[i]} - LT {edges[i + 1]}" for i in range(len(edges) - 1)]
    labels[0] = f"1. LT {edges[1]}"
    labels[-1] = f"{len(edges)-1}. GE {edges[-2]}"

    _var = df[var].copy()
    _var[_var <= 0] = np.nan

    df[f"Transform_{var}"] = pd.cut(_var, bins=edges, labels=labels, right=False, include_lowest=True)
    df[f"Transform_{var}"] = df[f"Transform_{var}"].cat.add_categories("NA").fillna("NA")
    print(df[f"Transform_{var}"].value_counts().sort_index())

def fn_crosstab(df, var_list, resp): # 2x2 contingency table analysis
    all_pairs = list(combinations(var_list, 2))
    for var1, var2 in all_pairs:
        print(f"=== Analysing 2x2 contingency table for {var1} x {var2} ===")
        ct_resp = pd.crosstab(index=df[var1], columns=df[var2], values=df[resp], aggfunc="mean")
        ct_resp_pct = (ct_resp * 100).round(1).fillna(0)
        ct_vol_pct = (pd.crosstab(index=df[var1], columns=df[var2], normalize="all") * 100).round(1)

        fig, axes = plt.subplots(1, 2, figsize=(14, 6))
        fig.suptitle(f"Contingency Analysis: {var1} vs {var2}", fontsize=16, fontweight="bold")

        sns.heatmap(ct_resp_pct, annot=True, fmt=".1f", cmap="RdYlGn", cbar=True,
            ax=axes[0], linewidths=0.5, annot_kws={"size": 12, "weight": "bold"})
        axes[0].set_title("Response Rates (%)", fontsize=12, fontweight="bold")
        axes[0].set_ylabel(var1, fontsize=10)
        axes[0].set_xlabel(var2, fontsize=10)

        sns.heatmap(ct_vol_pct, annot=True, fmt=".1f", cmap="Blues", cbar=True,
            ax=axes[1], linewidths=0.5, annot_kws={"size": 12, "weight": "bold"})
        axes[1].set_title("Observation Distribution (%)", fontsize=12, fontweight="bold")
        axes[1].set_ylabel(var1, fontsize=10)
        axes[1].set_xlabel(var2, fontsize=10)

        plt.tight_layout()
        img_filename = f"{out_img}xtab_{var1}_{var2}.png"
        plt.savefig(img_filename, dpi=300)
        plt.close()

def fn_woe_iv(df, var, resp): # Calculating WOE and IV for a categorial variable
    print(f"=== Calculating WoE IV for {var} ===")
    df_clean = df[[var, resp]].copy()

    # Convert to string and fill missing values with "NA"
    df_clean[var] = df_clean[var].fillna("NA").astype(str).replace(["", "nan", "None"], "NA")

    # Baseline totals
    total_events = df_clean[resp].sum()  # Total Survived (1s)
    total_non_events = len(df_clean) - total_events  # Total Dead (0s)

    # Calculate WOE and IV
    woe_table = df_clean.groupby(var)[resp].agg(counts="count", events="sum")
    woe_table["non_events"] = woe_table["counts"] - woe_table["events"]
    woe_table["prop_events"] = woe_table["events"] / total_events
    woe_table["prop_non_events"] = woe_table["non_events"] / total_non_events

    # Epsilon safeguard to protect against log(0) errors if a group has 0 events
    eps = 1e-9
    woe_table["WoE"] = np.log((woe_table["prop_non_events"] + eps) / (woe_table["prop_events"] + eps))
    woe_table["IV"] = ( woe_table["prop_non_events"] - woe_table["prop_events"] ) * woe_table["WoE"]

    total_iv = woe_table["IV"].sum()
    return woe_table, total_iv

def fn_corr_matrix(df, var_list): # Evaluate linear correction between numeric variables
    numeric_features = df[var_list].dropna()
    corr_matrix = numeric_features.corr(method="pearson")

    plt.figure(figsize=(8, 6))
    sns.heatmap(corr_matrix, annot=True, fmt=".2f", cmap="coolwarm", vmin=-1, vmax=1)
    plt.title("Predictor Correlation Matrix")
    plt.tight_layout()
    img_filename = f"{out_img}corr_matrix.png"
    plt.savefig(img_filename, dpi=300)
    plt.close()

def fn_vif(df, var_list):
    # drop_first=True prevents the 'dummy variable trap' multicollinearity
    x = pd.get_dummies(df[var_list], drop_first=True, dtype=int).dropna()
    x.insert(0, "Intercept", 1)

    vif_data = pd.DataFrame()
    vif_data["Feature"] = x.columns
    vif_data["VIF"] = [variance_inflation_factor(x.values, i) for i in range(x.shape[1])]

    print("=== VARIANCE INFLATION FACTOR (VIF) RESULTS ===")
    print(vif_data)


#----------------------------- READIN DATA -----------------------------#
print("=== Read-in modelling dataset ===")
df_mdl = pd.read_csv(dsn_mdl)
df_mdl.drop(columns=drop_cols, inplace=True)

#------------------------------ ANALYSIS -------------------------------#
print("=== Generating statistics for numeric variables ===")
df_stats = df_mdl[var_num].describe().loc[["mean", "std", "min", "50%", "max", "count"]]
df_stats.loc["missing"] = df_mdl[var_num].isna().sum()
df_stats.index = ["mean", "std dev", "min", "median", "max", "count", "missing"]
df_stats = df_stats.T
df_stats.index.name = "variable name"
df_stats = df_stats.reset_index()

df_stats.to_csv(out_num, index=False)
print(df_stats)

print("=== Generating value counts for categorical variables ===")
df_counts = []

for var in var_ctg:
    _counts = df_mdl[var].value_counts().reset_index()
    _counts.columns = ["level", "counts"]
    _counts.insert(0, "variable name", var)
    df_counts.append(_counts)

df_counts = pd.concat(df_counts, ignore_index=True)
df_counts = df_counts.sort_values(by=["variable name", "level"])
df_counts.to_csv(out_ctg, index=False)
print(df_counts)

# Create histograms for one numeric variable
fn_num_hist(df_mdl, "Age", 8)

# Optimal binning for one numeric variable
# New binned variable with Transform_ prefix
fn_opt_bin(df_mdl, "Age", var_resp)

# Other variable transformation
# 1. SibSp: put into 2 groups; 0, 1+
df_mdl["Transform_SibSp"] = df_mdl["SibSp"].clip(upper=1)
print("=== Transforming SibSp ===")
print(df_mdl["Transform_SibSp"].value_counts().sort_index())

# 2. Parch: put into 3 groups; 0, 1, 2+
df_mdl["Transform_Parch"] = df_mdl["Parch"].clip(upper=2)
print("=== Transforming Parch ===")
print(df_mdl["Transform_Parch"].value_counts().sort_index())

# 3. Calculate Family_Size (including the passenger themselves)
df_mdl["Family_Size"] = df_mdl["SibSp"] + df_mdl["Parch"] + 1
print("=== Creating a new variable: Family Size ===")
print(df_mdl["Family_Size"].value_counts().sort_index())

# 4. Family_Size: put into 4 groups; 1, 2, 3, 4+
df_mdl["Transform_Family"] = df_mdl["Family_Size"].clip(upper=4)
print("=== Transforming Family Size ===")
print(df_mdl["Transform_Family"].value_counts().sort_index())

# 2x2 Contingency table - results saved as image files
# Manually examine output heatmaps to justify including
transform_vars = [col for col in df_mdl.columns if col.startswith("Transform_")]
fn_crosstab(df_mdl, var_ctg + transform_vars, var_resp)
xtab_vars = []   # list any cross variables to be included in WoE/IV

# WoE and Information Value for a list of categorical variables
add_var = ["Family_Size"]    # any new variables to be included
iv_summary = {}
woe_var = []
transform_vars = [col for col in df_mdl.columns if col.startswith("Transform_")]
for _var in var_ctg + transform_vars + xtab_vars + add_var:
    if _var in df_mdl.columns:
        table, iv_score = fn_woe_iv(df_mdl, _var, var_resp)

        table_reset = table.reset_index()
        table_reset.insert(0, "Variable", _var)
        table_reset = table_reset.rename(columns={_var: "Category"})
        woe_var.append(table_reset)
        total_row = pd.DataFrame({"Variable": [_var], "Category": ["Total"], "IV": [iv_score]})
        woe_var.append(total_row)

        iv_summary[_var] = iv_score

print("=============================================")
print("     FINAL ATTRIBUTE PREDICTIVE RANKING      ")
print("=============================================")
sorted_ranking = sorted(iv_summary.items(), key=lambda item: item[1], reverse=True)
for rank, (feature, iv) in enumerate(sorted_ranking, 1):
    print(f"Rank {rank}: {feature:<15} -> IV: {iv:.4f}")

woe_all = pd.concat(woe_var, ignore_index=True)
woe_all.to_csv(out_woe, index=False)

# Multicollinearity - Correlation Matrix and The Variance Inflation Factor
fn_corr_matrix(df_mdl, var_num + ["Family_Size"])
fn_vif(df_mdl, ["Pclass", "Family_Size", "Sex", "Embarked", "Transform_Age"])

#------------------ MODELLING: Training + Validation -------------------#
# 1. Data Preparation
selected_features = ["Pclass", "Family_Size", "Sex", "Embarked", "Transform_Age"]
model_df = df_mdl[selected_features + [var_resp]].dropna().copy()
X_encoded = pd.get_dummies(
    model_df[selected_features],
    columns=["Sex", "Embarked", "Transform_Age"],
    drop_first=True,
    dtype=int
)
y = model_df[var_resp]

# 2. 80/20 stratified split to get training and validation datasets
X_train, X_val, y_train, y_val = train_test_split(
    X_encoded,
    y,
    test_size=0.20,
    random_state=42,
    stratify=y
)

# 2. Define the Modeling Suite
models = {
    "Logistic Regression": LogisticRegression(C=1e9, max_iter=1000),
    "Random Forest": RandomForestClassifier(n_estimators=100, max_depth=5, random_state=42),
    "Gradient Boosting": GradientBoostingClassifier(n_estimators=100, max_depth=3, random_state=42),
    "Support Vector Machine (SVM)": Pipeline([('scaler', StandardScaler()),
                                              ('svc', SVC(kernel="rbf", C=1.0, random_state=42))]),
    "Gaussian Naive Bayes": GaussianNB(),
    "XGBoost": XGBClassifier(n_estimators=100, learning_rate=0.05, max_depth=4,
                             random_state=42, eval_metric='logloss'),
    "Neural Network (MLP)": Pipeline([('scaler', StandardScaler()),
                                      ('mlp', MLPClassifier(hidden_layer_sizes=(64, 32), max_iter=1000,
                                                            early_stopping=True, random_state=42))
                                      ])
}

# 3. Train on 80% and validate on 20%
train_summary = {}
val_summary = {}

print("=" * 80)
print("                TRAINING (80%) vs. VALIDATION (20%) BATCH RUN")
print("=" * 80)

for name, model_obj in models.items():
    # Train ONLY on the 80% training data
    model_obj.fit(X_train, y_train)

    # Predict & score on Training Data
    y_train_pred = model_obj.predict(X_train)
    t_acc = accuracy_score(y_train, y_train_pred)
    train_summary[name] = t_acc

    # Predict & score on validation data (the 20%)
    y_val_pred = model_obj.predict(X_val)
    v_acc = accuracy_score(y_val, y_val_pred)
    v_rmse = math.sqrt(mean_squared_error(y_val, y_val_pred))
    v_mae = mean_absolute_error(y_val, y_val_pred)
    val_summary[name] = (v_acc, v_rmse, v_mae)

    print(f"\n [ {name} ]")
    print(f"   Training Accuracy:   {t_acc:.4f}  |  Validation Accuracy: {v_acc:.4f}")
    print("-" * 80)

# 4. Print validation leaderboard
print("\n" + "=" * 80)
print("           TRUE UNBIASED LEADERBOARD (SORTED BY VALIDATION ACCURACY)")
print("=" * 80)
print(f" {'Model Name':<36} | {'Train Acc':>11} | {'Val Acc':>11} | {'Val RMSE':>11}")
print("-" * 80)

# Sorting the final output by the 20% validation chunk results
sorted_val_leaderboard = sorted(val_summary.items(), key=lambda item: item[1][0], reverse=True)

for rank, (model_name, metrics) in enumerate(sorted_val_leaderboard, 1):
    v_accuracy, v_rmse, _ = metrics
    t_accuracy = train_summary[model_name]

    rank_str = f"Rank {rank}: {model_name}"
    print(f" {rank_str:<36} | {t_accuracy:>11.4f} | {v_accuracy:>11.4f} | {v_rmse:>11.4f}")
print("=" * 80)

#---------------- RE- MODELLING: 100% of training data -----------------#
model_summary = {}

print("=" * 80)
print("                      RE-TRAINING FULL DATSET BATCH RUN")
print("=" * 80)

for name, model_obj in models.items():
    model_obj.fit(X_encoded, y)

    # Predict & score on full dataset
    y_pred = model_obj.predict(X_encoded)
    acc = accuracy_score(y, y_pred)
    rmse= math.sqrt(mean_squared_error(y, y_pred))
    mae = mean_absolute_error(y, y_pred)
    model_summary[name] = (acc, rmse, mae)

print("\n" + "=" * 80)
print("           MODELLING LEADERBOARD")
print("=" * 80)
print(f" {'Model Name':<36} | {'Acc':>11} | {'RMSE':>11} | {'MAE':>11}")
print("-" * 80)

# Sorting the final output by the 20% validation chunk results
sorted_leaderboard = sorted(model_summary.items(), key=lambda item: item[1][0], reverse=True)

for rank, (model_name, metrics) in enumerate(sorted_leaderboard, 1):
    accuracy, rmse, mae = metrics

    rank_str = f"Rank {rank}: {model_name}"
    print(f" {rank_str:<36} | {accuracy:>11.4f} | {rmse:>11.4f} | {mae:>11.4f}")
print("=" * 80)

#-------------- PREDICT TEST DATASET USING SVM and LOGIT ---------------#
# 1. Data Preparation
print("=== Read-in validation dataset ===")
df_test = pd.read_csv(dsn_tst)
df_test.drop(columns=drop_cols, inplace=True)

age_splits = [6.5, 17.5, 26.5, 36.25]
edges = [-np.inf] + list(age_splits) + [np.inf]
labels = [f"{i + 1}. {edges[i]} - LT {edges[i + 1]}" for i in range(len(edges) - 1)]
labels[0] = f"1. LT {edges[1]}"
labels[-1] = f"{len(edges) - 1}. GE {edges[-2]}"

_var = df_test["Age"].copy()
_var[_var <= 0] = np.nan

df_test[f"Transform_Age"] = pd.cut(_var, bins=edges, labels=labels, right=False, include_lowest=True)
df_test[f"Transform_Age"] = df_test[f"Transform_Age"].cat.add_categories("NA").fillna("NA")
print("=== Transforming variable Age ===")
print(df_test[f"Transform_Age"].value_counts().sort_index())

df_test["Family_Size"] = df_test["SibSp"] + df_test["Parch"] + 1
print("=== Creating a new variable: Family Size ===")
print(df_test["Family_Size"].value_counts().sort_index())

test_df_cleaned = df_test[selected_features].copy()

# One-hot encode the test dataset
X_test_encoded = pd.get_dummies(
    test_df_cleaned[selected_features],
    columns=["Sex", "Embarked", "Transform_Age"],
    drop_first=True,
    dtype=int
)
X_test_encoded = X_test_encoded.reindex(columns=X_encoded.columns, fill_value=0)

# 2. Extract pre-trained SVM & predict
svm_champion = models["Support Vector Machine (SVM)"]
reg_champion = models["Logistic Regression"]

print("Generating predictions for test dataset using pre-trained SVM...")
test_pred_svm = svm_champion.predict(X_test_encoded)
print("Generating predictions for test dataset using pre-trained SVM...")
test_pred_reg = reg_champion.predict(X_test_encoded)

# 3. Create Kaggle submission file
sub_svm = pd.DataFrame({"PassengerId": df_test["PassengerId"], "Survived": test_pred_svm})
sub_reg = pd.DataFrame({"PassengerId": df_test["PassengerId"], "Survived": test_pred_reg})

# Save to your local directory
sub_svm.to_csv("data/titanic_sub_svm.csv", index=False)
sub_reg.to_csv("data/titanic_sub_reg.csv", index=False)

print("\n" + "="*50)
print(f"     SUCCESS:  data/titanic_sub_svm.csv is ready!")
print(f"     Total rows predicted: {len(sub_svm)}")
print("")
print(f"     SUCCESS:  data/titanic_sub_reg.csv is ready!")
print(f"     Total rows predicted: {len(sub_reg)}")
print("="*50)
