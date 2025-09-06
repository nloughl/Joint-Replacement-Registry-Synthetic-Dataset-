import pandas as pd
from collections import defaultdict

# Pick one column to test with first
test_column = 'your_column_name_here'  # Replace with actual column name

print(f"Testing with column: {test_column}")
print(f"Missing values in {test_column}: {df[test_column].isnull().sum()}")
print(f"Non-missing values in {test_column}: {df[test_column].notnull().sum()}")

# Create dictionary manually for one column
test_mapping = {}
for catalogue_num in df['catalogue_number'].unique():
    # Get all rows for this catalogue number
    catalogue_rows = df[df['catalogue_number'] == catalogue_num]
    # Get non-null values for our test column
    non_null_values = catalogue_rows[test_column].dropna()
    
    if len(non_null_values) > 0:
        test_mapping[catalogue_num] = non_null_values.iloc[0]
        print(f"Catalogue {catalogue_num}: found value {non_null_values.iloc[0]}")
    else:
        test_mapping[catalogue_num] = None
        print(f"Catalogue {catalogue_num}: no values available")

print(f"\nCreated mapping with {len(test_mapping)} entries")
print(f"Non-null mappings: {sum(1 for v in test_mapping.values() if pd.notnull(v))}")

# Test filling for just this one column
print(f"\nBefore filling: {df[test_column].isnull().sum()} missing values")

# Create a copy to test on
df_test = df.copy()

# Fill missing values
df_test[test_column] = df_test[test_column].fillna(df_test['catalogue_number'].map(test_mapping))

print(f"After filling: {df_test[test_column].isnull().sum()} missing values")
print(f"Values filled: {df[test_column].isnull().sum() - df_test[test_column].isnull().sum()}")


#================ APPLY TO COLUMNS OF CHOICE ==================
df = df_IPL
columns_to_backfill = ['Fixation', 'Size', 'Length', 'Stability', 'Mobility', 'ML', 'MMat', 'PMat', 'StemDesign', 'AP', 'Thick']
# Create dictionaries for each column you want to backfill
catalogue_mappings = {}
for column in columns_to_backfill:
    catalogue_mappings[column] = df.groupby('catalogue_number')[column].apply(
        lambda x: x.dropna().iloc[0] if len(x.dropna()) > 0 else None
    ).to_dict()

# Fill missing values
for column in columns_to_backfill:
    df[column] = df[column].fillna(df['catalogue_number'].map(catalogue_mappings[column]))



# Before imputation
print("Missing data BEFORE dictionary imputation:")
missing_before = df.isnull().sum()
print(missing_before[missing_before > 0])
print(f"\nTotal missing values: {df.isnull().sum().sum()}")
print(f"Missing data percentage: {(df.isnull().sum().sum() / df.size) * 100:.2f}%")

# After imputation
print("\nMissing data AFTER dictionary imputation:")
missing_after = df.isnull().sum()
print(missing_after[missing_after > 0])
print(f"\nTotal missing values: {df.isnull().sum().sum()}")
print(f"Missing data percentage: {(df.isnull().sum().sum() / df.size) * 100:.2f}%")