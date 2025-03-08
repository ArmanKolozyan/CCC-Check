import os
import subprocess
import re
import time

# constants
PICUS_COMMAND = "./run-picus"
BENCHMARKS_DIR = "./benchmarks/"
TIMEOUT_SECONDS = 180  # 3 minutes timeout per benchmark

def run_picus(file_path):
    """Runs Picus on the given file with a timeout and returns the output."""
    try:
        result = subprocess.run(
            f"{PICUS_COMMAND} {file_path}",
            shell=True, capture_output=True, text=True, timeout=TIMEOUT_SECONDS
        )
        return result.stdout
    except subprocess.TimeoutExpired:
        print(f"Timeout: {file_path} exceeded {TIMEOUT_SECONDS} seconds")
        return f"Timeout: {file_path} exceeded {TIMEOUT_SECONDS} seconds"
    except Exception as e:
        print(f"Error running Picus on {file_path}: {e}")
        return None

def extract_variable_ranges(output):
    """
    Extracts the variable range information from Picus output.
    Returns a dictionary {variable_name: possible_values}.
    """
    variable_ranges = {}
    range_pattern = re.compile(r"possible values for x(\d+): (.+)")

    for line in output.splitlines():
        match = range_pattern.match(line)
        if match:
            var_id, values = match.groups()
            variable_ranges[f"x{var_id}"] = values

    return variable_ranges

def count_restricted_ranges(variable_ranges):
    """Counts number of variables that do not have 'bottom' as a value."""
    return sum(1 for values in variable_ranges.values() if values != 'bottom')

def process_benchmark(file_path, output_file):
    """Runs Picus on a file and processes the variable range data."""
    print(f"Processing: {file_path}")
    output = run_picus(file_path)
    
    if output is None:
        return None

    variable_ranges = extract_variable_ranges(output)
    restricted_count = count_restricted_ranges(variable_ranges)
    total_vars = len(variable_ranges)

    # saving results
    with open(output_file, "a") as f:
        f.write(f"### Benchmark: {file_path}\n")
        for var, values in variable_ranges.items():
            f.write(f"{var}: {values}\n")
        f.write(f"Restricted variables: {restricted_count}/{total_vars}\n")
        f.write("\n" + "="*80 + "\n\n")

def main():
    """Iterates through each outermost folder in benchmarks, runs picus on all the 
    (nested) files, and saves the variable analysis information."""
    start_time = time.time()
    
    for folder in os.listdir(BENCHMARKS_DIR):
        folder_path = os.path.join(BENCHMARKS_DIR, folder)
        if folder != 'lib' and os.path.isdir(folder_path):
            output_file = f"variable_ranges_{folder}.txt" 
            
            with open(output_file, "w") as f:
                f.write(f"Picus Variable Ranges Analysis for {folder}\n")
                f.write("="*80 + "\n\n")
            
            for root, _, files in os.walk(folder_path):
                for file in files:
                    if file.endswith((".circom", ".r1cs", ".sr1cs")):
                        file_path = os.path.join(root, file)
                        process_benchmark(file_path, output_file)
    
    print("All benchmarks processed.")
    print(f"Total execution time: {time.time() - start_time:.2f} seconds")

if __name__ == "__main__":
    main()