#!/usr/bin/env python3

import pandas as pd
import numpy as np
import argparse
import sys
from pathlib import Path


## calculates comprehensive statistics for timing data
def calculate_statistics(times):
    times_array = np.array(times)
    
    stats = {
        'mean': np.mean(times_array),
        'std_dev': np.std(times_array, ddof=1),
        'min': np.min(times_array),
        'max': np.max(times_array),
        'median': np.median(times_array),
        'q25': np.percentile(times_array, 25),
        'q75': np.percentile(times_array, 75),
        'samples': len(times_array)
    }
    
    return stats

## analyzes benchmark data from a CSV file
def analyze_benchmark_data(csv_file, warmup_runs=0):
    
    # checking if file exists
    if not Path(csv_file).exists():
        print(f"Error: CSV file not found: {csv_file}")
        sys.exit(1)
    
    # creating statistics directory if it doesn't exist
    Path("data/statistics").mkdir(parents=True, exist_ok=True)
    
    # reading CSV data
    try:
        df = pd.read_csv(csv_file)
    except Exception as e:
        print(f"Error reading CSV file: {e}")
        sys.exit(1)
    
    # validating required columns
    required_cols = ['circuit', 'run', 'time_ms', 'is_warmup']
    if not all(col in df.columns for col in required_cols):
        print(f"Error: CSV missing required columns. Expected: {required_cols}")
        sys.exit(1)
    
    # filtering out warmup runs for analysis
    analysis_data = df[df['is_warmup'] == False]['time_ms'].tolist()
    
    if len(analysis_data) == 0:
        print("Error: No data available for analysis after filtering warmup runs.")
        sys.exit(1)
    
    # calculating statistics
    stats = calculate_statistics(analysis_data)
    
    # displaying results
    circuit_name = df['circuit'].iloc[0]
    total_runs = len(df)
    warmup_count = len(df[df['is_warmup'] == True])
    
    print(f"📊 Statistical Analysis for: {circuit_name}")
    print("=" * 50)
    print(f"Total runs: {total_runs}")
    print(f"Warmup runs excluded: {warmup_count}")
    print(f"Analysis samples: {stats['samples']}")
    print()
    print("⏱️ Timing Statistics:")
    print(f" Mean:    {stats['mean']:.3f} ms")
    print(f" Std Dev: {stats['std_dev']:.3f} ms")
    print(f" Median:  {stats['median']:.3f} ms")
    print(f" Min:     {stats['min']:.3f} ms")
    print(f" Max:     {stats['max']:.3f} ms")
    print(f" Q25:     {stats['q25']:.3f} ms")
    print(f" Q75:     {stats['q75']:.3f} ms")
    print()
    
    # additional insights
    cv = (stats['std_dev'] / stats['mean']) * 100  # coefficient of variation
    print("Additional Insights:")
    print(f"   Coefficient of Variation: {cv:.2f}%")
    print(f"   Range: {stats['max'] - stats['min']:.3f} ms")
    
    if cv < 5:
        print("✅  Low variability")
    elif cv < 15:
        print("⚠️  Moderate variability")
    else:
        print("🔴  High variability: need to maybe consider more warmup runs")
    
    return stats, df

def main():
    parser = argparse.ArgumentParser(description="Analyze CIVER benchmark timing data")
    parser.add_argument("csv_file", help="CSV file containing benchmark results")
    parser.add_argument("--warmup", type=int, default=0, 
                       help="Number of warmup runs to exclude (default: 0)")
    
    args = parser.parse_args()
    
    # analyzing the data
    stats, df = analyze_benchmark_data(args.csv_file, args.warmup)
    
    # exporting statistics to JSON
    csv_filename = Path(args.csv_file).stem
    # removing timestamp from filename if present
    circuit_base_name = csv_filename.replace('benchmark_results_', '').split('_')[0] if 'benchmark_results_' in csv_filename else csv_filename
    json_file = f"data/statistics/{circuit_base_name}.json"
    
    import json
    detailed_stats = {
        'circuit': df['circuit'].iloc[0],
        'total_runs': len(df),
        'warmup_runs': len(df[df['is_warmup'] == True]),
        'analysis_samples': stats['samples'],
        'statistics': {
            'mean_ms': round(stats['mean'], 3),
            'std_dev_ms': round(stats['std_dev'], 3),
            'median_ms': round(stats['median'], 3),
            'min_ms': round(stats['min'], 3),
            'max_ms': round(stats['max'], 3),
            'q25_ms': round(stats['q25'], 3),
            'q75_ms': round(stats['q75'], 3),
            'coefficient_of_variation_percent': round((stats['std_dev'] / stats['mean']) * 100, 2),
            'range_ms': round(stats['max'] - stats['min'], 3)
        }
    }
    
    with open(json_file, 'w') as f:
        json.dump(detailed_stats, f, indent=2)
    
    print(f"📊 Detailed statistics exported to: {json_file}")

if __name__ == "__main__":
    main()
