import pandas as pd
import numpy as np
import random

# Set seed for reproducibility
np.random.seed(42)
random.seed(42)

# Constants
n_rows = 100

# Dataset 1: Patient and Surgery Information
dataset1 = pd.DataFrame({
    "De-identified Joint ID": [f"JNT{str(i).zfill(4)}" for i in range(n_rows)],
    "De-identified Patient ID": [f"PAT{str(random.randint(1, 30)).zfill(4)}" for _ in range(n_rows)],
    "Patient Age": np.random.randint(50, 85, n_rows),
    "Patient Sex": np.random.choice(["Male", "Female"], n_rows),
    "Province/Territory": np.random.choice(["ON", "BC", "AB", "QC", "MB", "NS"], n_rows),
    "Data source": np.random.choice(["DAD", "NACRS"], n_rows),
    "Fiscal year": np.random.choice(["2018/19", "2019/20", "2020/21", "2021/22"], n_rows),
    "Surgery Year": np.random.choice([2018, 2019, 2020, 2021], n_rows),
    "Replacement Type": np.random.choice(["Primary", "Revision"], n_rows, p=[0.8, 0.2]),
    "Primary Intervention code": np.random.choice(["1VA53LAPN", "1VA53LLPN", "1VA53RAPN"], n_rows),
    "Primary Status attribute": np.random.choice(["Total", "Partial"], n_rows),
    "Primary Location attribute": np.random.choice(["Unilateral", "Bilateral"], n_rows),
    "Primary replacement side": np.random.choice(["Left", "Right", "Both"], n_rows),
    "Primary procedure type": np.random.choice(["Total Knee Arthroplasty", "Unicompartmental Arthroplasty"], n_rows),
    "Most responsible diagnosis": np.random.choice(["Osteoarthritis", "Rheumatoid Arthritis", "Post-traumatic Arthritis"], n_rows),
    "Teaching Hospital Flag": np.random.choice(["Yes", "No"], n_rows),
    "Cement Flag (CJRR)": np.random.choice(["Yes", "No", "Unknown"], n_rows),
    "Cement Flag (CCI)": np.random.choice(["Yes", "No", "Unknown"], n_rows)
})

# Dataset 2: Prosthesis Information
components = ["Femoral Component", "Tibial Insert", "Patella"]
manufacturers = ["Zimmer", "Stryker", "DePuy", "Smith & Nephew"]
component_map = {"Femoral Component": "Femoral", "Tibial Insert": "Tibial", "Patella": "Patellar"}
component_names = ["Persona", "Triathlon", "PFC Sigma", "Genesis II"]
prosthesis_data = []

for i in range(n_rows):
    joint_id = dataset1.loc[i, "De-identified Joint ID"]
    for comp in components:
        manufacturer = random.choice(manufacturers)
        component_name = random.choice(component_names)
        cat_num = f"CAT{random.randint(1000, 9999)}"
        prosthesis_data.append({
            "De-identified primary Joint ID": joint_id,
            "Joint Component": comp,
            "ComponentType": component_map[comp],
            "Prosthesis Catalogue Number": cat_num,
            "Prosthesis GTIN Number": f"{random.randint(10000000000000, 99999999999999)}",
            "Manufacturer Name": manufacturer,
            "ComponentDescription": f"{comp} component for knee replacement",
            "ComponentName": component_name,
            "GMDN": f"{random.randint(10000, 99999)}",
            "Industry": manufacturer,
            "Manufacturer": manufacturer,
            "ManufacturerReported": random.choice([1, 0]),
            "ModelName": f"{component_name} {random.randint(1, 100)}",
            "ProductName": component_name,
            "UDI": f"{random.randint(10000000000000, 99999999999999)}-{random.randint(100, 999)}",
            "Antioxidant": np.random.choice(["Vitamin E", "None", "CROSS", "Unknown"]),
            "ComponentSetting": np.random.choice(["Fixed", "Mobile", "Not Applicable"]),
            "DesignArticularSurface": np.random.choice(["Flat", "Conforming", "Posterior-Stabilized"]),
            "DesignFixationSurface": np.random.choice(["Porous", "Rough", "Smooth"]),
            "DesignPatellaArticularSurface": np.random.choice(["Dome", "Anatomic"]),
            "DesignPatellaFixationSurface": np.random.choice(["Pegged", "Flat"]),
            "AP": round(random.uniform(30.0, 50.0), 1),
            "Diameter": round(random.uniform(20.0, 40.0), 1),
            "Length": round(random.uniform(30.0, 60.0), 1),
            "ML": round(random.uniform(20.0, 50.0), 1),
            "Thick": round(random.uniform(6.0, 12.0), 1),
            "Fixation": np.random.choice(["Cemented", "Cementless"]),
            "FixationSurfaceCemented": np.random.choice(["Coated", "Rough", "Polished"]),
            "FixationSurfaceCementless": np.random.choice(["Porous", "Textured", "Sintered"]),
            "PatellaBaseplateInsertCombined ManufactureCemented": np.random.choice(["Yes", "No"]),
            "PatellaBaseplateInsertCombined ManufactureCementless": np.random.choice(["Yes", "No"]),
            "TibialBaseplateInsertCombined ManufactureCemented": np.random.choice(["Yes", "No"]),
            "TibialBaseplateInsertCombined ManufactureCementless": np.random.choice(["Yes", "No"]),
            "FlexionCapacity": np.random.choice(["Standard", "High Flex"]),
            "Hierarchy": np.random.choice(["Major", "Minor"]),
            "CMMat": np.random.choice(["Oxinium", "None"]),
            "MMat": np.random.choice(["CoCr", "Titanium"]),
            "PMat": np.random.choice(["UHMWPE", "XLPE"]),
            "Stability": np.random.choice(["CR", "PS", "CS"]),
            "TotalSide": np.random.choice(["Left", "Right", "Both"]),
            "UnicompartmentalSide": np.random.choice(["Medial", "Lateral", "Unknown"]),
            "Size": np.random.choice(["Small", "Medium", "Large", "Size 1", "Size 3", "Size 5"]),
            "Surf_TreatmentBearSurf": np.random.choice(["Polished", "Textured", "Porous"]),
            "Surf_TreatmentFixationSurf": np.random.choice(["Rough", "Porous", "None"]),
            "Surf_TreatmentInterface SurfaceWInsert": np.random.choice(["Smooth", "Textured", "Unknown"]),
            "StemDesign": np.random.choice(["Monoblock", "Modular", "Short Stem"]),
            "JointType": "KNEE",
            "sku": manufacturer[:3].upper() + "_" + cat_num,
            "Enabled": np.random.choice(["Yes", "No"])
        })

# Finalize Prosthesis DataFrame
dataset2 = pd.DataFrame(prosthesis_data)

# Output sample
dataset1.head(), dataset2.head()

# Export dfs to .csv (so they can be read into R for analysis)
dataset1.to_csv("dataset1.csv", index=False)
dataset2.to_csv("dataset2.csv", index=False)



