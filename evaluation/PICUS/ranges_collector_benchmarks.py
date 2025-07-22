import os
import subprocess
import re
import time

# constants
PICUS_COMMAND = "./run-picus"
CIVER_BENCHMARKS_DIR = "../../evaluation/CIVER-benchmarks/"
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
    
    # updated pattern to capture any variable name, not just x followed by digits
    range_pattern = re.compile(r"possible values for ([^:]+): (.+)")

    for line in output.splitlines():
        match = range_pattern.match(line)
        if match:
            var_name, values = match.groups()
            var_name = var_name.strip()  # Remove any extra whitespace
            variable_ranges[var_name] = values

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

    return restricted_count, total_vars

def main():
    """Runs Picus on all .circom files in the CIVER-benchmarks directory."""
    start_time = time.time()
    
    output_file = "ranges_20_benchmarks.txt"
    
    with open(output_file, "w") as f:
        f.write("Picus Variable Ranges Analysis for 20 Benchmark Programs\n")
        f.write("="*80 + "\n\n")
    
    total_restricted = 0
    total_variables = 0
    total_files = 0
    
    # processing all .circom files in the CIVER-benchmarks directory
    for file in os.listdir(CIVER_BENCHMARKS_DIR):
        if file.endswith(".circom"):
            file_path = os.path.join(CIVER_BENCHMARKS_DIR, file)
            result = process_benchmark(file_path, output_file)
            if result:
                restricted_count, var_count = result
                total_restricted += restricted_count
                total_variables += var_count
                total_files += 1
    
    # writing summary
    with open(output_file, "a") as f:
        f.write("\n" + "="*80 + "\n")
        f.write("SUMMARY\n")
        f.write("="*80 + "\n")
        f.write(f"Total files processed: {total_files}\n")
        f.write(f"Total variables: {total_variables}\n")
        f.write(f"Total restricted variables: {total_restricted}\n")
        f.write(f"Percentage of variables with restricted ranges: {(total_restricted/total_variables)*100:.2f}%\n")
    
    print(f"All {total_files} program files processed.")
    print(f"Total restricted variables: {total_restricted}/{total_variables} ({(total_restricted/total_variables)*100:.2f}%)")
    print(f"Total execution time: {time.time() - start_time:.2f} seconds")
    print(f"Results saved to: {output_file}")

if __name__ == "__main__":
    main()
