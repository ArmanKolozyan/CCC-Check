import json
import os
from pathlib import Path
from datetime import datetime

def aggregate_stats():
    """Aggregates all JSON statistics files into a single text report."""
    
    # getting the statistics directory path
    stats_dir = Path("data/statistics")
    
    if not stats_dir.exists():
        print("Error: data/statistics directory not found")
        return
    
    # finding all JSON files in the statistics directory
    json_files = list(stats_dir.glob("*.json"))
    
    if not json_files:
        print("No JSON statistics files found in data/statistics/")
        return
    
    # sorting
    json_files.sort(key=lambda x: x.stem)
    
    # creating aggregated report
    output_file = stats_dir / "aggregated_report.txt"
    
    with open(output_file, 'w') as f:
        # writing header
        f.write("=" * 80 + "\n")
        f.write("CIVER BENCHMARK AGGREGATED STATISTICS REPORT\n")
        f.write("=" * 80 + "\n")
        f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write(f"Total circuits analyzed: {len(json_files)}\n")
        f.write("=" * 80 + "\n\n")
        
        # processing each JSON file
        all_stats = []
        for json_file in json_files:
            try:
                with open(json_file, 'r') as jf:
                    data = json.load(jf)
                    all_stats.append(data)
                    
                    # writing individual circuit report
                    f.write(f"Circuit: {data['circuit']}\n")
                    f.write("-" * 40 + "\n")
                    f.write(f"Total runs: {data['total_runs']}\n")
                    f.write(f"Warmup runs: {data['warmup_runs']}\n")
                    f.write(f"Analysis samples: {data['analysis_samples']}\n")
                    f.write("\nTiming Statistics:\n")
                    
                    stats = data['statistics']
                    f.write(f"  Mean:       {stats['mean_ms']:.3f} ms\n")
                    f.write(f"  Std Dev:    {stats['std_dev_ms']:.3f} ms\n")
                    f.write(f"  Median:     {stats['median_ms']:.3f} ms\n")
                    f.write(f"  Min:        {stats['min_ms']:.3f} ms\n")
                    f.write(f"  Max:        {stats['max_ms']:.3f} ms\n")
                    f.write(f"  Q25:        {stats['q25_ms']:.3f} ms\n")
                    f.write(f"  Q75:        {stats['q75_ms']:.3f} ms\n")
                    f.write(f"  CV:         {stats['coefficient_of_variation_percent']:.2f}%\n")
                    f.write(f"  Range:      {stats['range_ms']:.3f} ms\n")
                    
                    # performance assessment
                    cv = stats['coefficient_of_variation_percent']
                    if cv < 5:
                        assessment = "✅ Low variability - consistent performance"
                    elif cv < 15:
                        assessment = "⚠️  Moderate variability"
                    else:
                        assessment = "🔴 High variability - need to consider more warmup runs"
                    
                    f.write(f"  Assessment: {assessment}\n")
                    f.write("\n")
                    
            except Exception as e:
                f.write(f"Error processing {json_file}: {e}\n\n")
        
        # writing summary statistics
        if all_stats:
            f.write("=" * 80 + "\n")
            f.write("SUMMARY STATISTICS ACROSS ALL CIRCUITS\n")
            f.write("=" * 80 + "\n")
            
            # calculating overall statistics
            all_means = [s['statistics']['mean_ms'] for s in all_stats]
            all_cvs = [s['statistics']['coefficient_of_variation_percent'] for s in all_stats]
            
            f.write(f"Overall mean execution times:\n")
            f.write(f"  Fastest circuit: {min(all_means):.3f} ms\n")
            f.write(f"  Slowest circuit: {max(all_means):.3f} ms\n")
            f.write(f"  Average across circuits: {sum(all_means)/len(all_means):.3f} ms\n")
            
            # circuit ranking by performance
            f.write(f"\nCircuit ranking by mean execution time:\n")
            sorted_circuits = sorted(all_stats, key=lambda x: x['statistics']['mean_ms'])
            for i, circuit in enumerate(sorted_circuits, 1):
                f.write(f"  {i:2d}. {circuit['circuit']:<20} {circuit['statistics']['mean_ms']:>8.3f} ms\n")
        
        f.write("\n" + "=" * 80 + "\n")
        f.write("END OF REPORT\n")
        f.write("=" * 80 + "\n")
    
    print(f"Aggregated report created: {output_file}")
    print(f"Processed {len(json_files)} circuit statistics")

if __name__ == "__main__":
    aggregate_stats()
