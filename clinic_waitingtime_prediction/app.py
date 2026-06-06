from flask import Flask, request, render_template
import joblib
import io
import pandas as pd
from cryptography.fernet import Fernet
from dotenv import load_dotenv
import os

# Step 3: Decrypt the model file (for use)
def decrypt_model(encrypted_file, env_file=".env"):
    # Load the encryption key from the .env file
    load_dotenv(env_file)
    encryption_key = os.getenv("ENCRYPTION_KEY")
    if not encryption_key:
        raise ValueError("Encryption key not found in .env file!")

    # Initialize the Fernet cipher with the key
    cipher_suite = Fernet(encryption_key.encode())

    # Read the encrypted model file
    with open(encrypted_file, "rb") as file:
        encrypted_data = file.read()

    # Decrypt the model data
    decrypted_data = cipher_suite.decrypt(encrypted_data)

    # Convert decrypted bytes into an in-memory stream for Joblib consumption
    buffer = io.BytesIO(decrypted_data)
    model = joblib.load(buffer)
    return model

app = Flask('IndigoWaitingTime')

@app.route('/', methods=['GET'])
def greet():
    return render_template('CheckWaitingTime.html')

@app.route('/show_waiting_time',methods=['POST'])
def show_waiting_time():
    # Convert form submissions to dictionary layout
    data = request.form.to_dict()

    # Base keys extracted directly from the HTML inputs
    featureList = ["NumDoctorsOnDuty",
                   "NumReceptionistsOnDuty",
                   "Temperature",
                   "Rainfall",
                   "PctPriorVacantSlots",
                   "PriorEmergency",
                   "AvgDoctorExperience",
                   "PctSwitch",
                   "AvgAgePrior"]

    # Extract and convert to float
    X = [float(data.get(f, 0)) for f in featureList]

    # Convert percentage values to proportions
    idx = featureList.index('PctPriorVacantSlots')
    X[idx] = X[idx] / 100
    idx = featureList.index('PctSwitch')
    X[idx] = X[idx] / 100

    # Build initial dataframe profile structure
    X_df = pd.DataFrame([X], columns=featureList)

    # Feature Engineering: Compute derived variable on the fly
    # (+1e-5 safety rail protects against division-by-zero if receptionists == 0)
    X_df['Workload_Ratio'] = X_df['NumDoctorsOnDuty'] / (X_df['NumReceptionistsOnDuty'] + 1e-5)

    # 1. Decrypt and load the pipeline dictionary package
    pipeline = decrypt_model('models/lightgbm_2stage_pipeline.enc')
    lgb_stage1 = pipeline["classifier"]
    lgb_stage2 = pipeline["regressor"]
    features_all = pipeline["features_all"]  # Fetch exact column sequences from file meta

    # 2. Enforce strict index alignment to match the exact tree column sequences
    X_df = X_df[features_all]
    print(X_df)  # For terminal diagnostic debugging

    # 3. Execute 2-stage LightLBM estimation calculations
    prob_of_wait = lgb_stage1.predict_proba(X_df)[0, 1]
    predicted_time = lgb_stage2.predict(X_df)[0]
    final_waiting_time = prob_of_wait * predicted_time

    # Optional safety rail: Ensure final time never drops below 0 due to minor noise
    final_waiting_time = max(0.0, final_waiting_time)

    return render_template('WaitingTime.html', waittime=round(final_waiting_time, 2))

if __name__ == '__main__':
    app.run(debug=True)
