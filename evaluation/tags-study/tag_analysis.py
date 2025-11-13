import os
import re
from collections import Counter
import glob

# Extracts all signal tags from Circom files and categorizes them by project.
def extract_tags_from_files(search_dir):
    
    # storing tags separately for each project so that we can do project-specific analysis
    tags_by_project = {
        'circomlib': [],
        'ecdsa': [], 
        'darkforest': []
    }
    all_tags = []
    
    # finding all .circom files recursively
    circom_files = glob.glob(os.path.join(search_dir, "**/*.circom"), recursive=True)
    
    print(f"Found {len(circom_files)} .circom files")
    
    for file_path in circom_files:
        # determining project based on directory path patterns
        project = 'other'
        if 'circomlib-only_adding_tags' in file_path:
            project = 'circomlib'
        elif 'circom-ecdsa-master' in file_path:
            project = 'ecdsa'
        elif 'darkforest-with-tags-main' in file_path:
            project = 'darkforest'
        
        file_tags = []
        
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
                
            # Pattern 1: curly brace tags: extracting from signal declarations like signal {binary}
            pattern1 = re.findall(r'signal\s*\{([^}]+)\}', content)
            for match in pattern1:
                tag_content = match.strip()
                
                # splitting multiple tags by comma
                tags = [t.strip() for t in tag_content.split(',')]
                for tag in tags:
                    # filtering out non-tag content like public declarations, loop indices, etc.
                    if (tag and 
                        not tag.startswith('public') and 
                        not re.match(r'[ij]\s*=', tag) and  # loop indices like "j = 0, ..., t-1"
                        not re.match(r'\w+\s*\[', tag) and  # array access like "in[0]"
                        tag not in ['public', 'private']):
                        # extracting just the tag name if it has additional syntax
                        clean_tag = tag.split()[0] if ' ' in tag else tag
                        file_tags.append(clean_tag)
            
            # Pattern 2: AddTag functions: extracting from function calls like AddBinaryTag()
            pattern2 = re.findall(r'Add(\w+)Tag\(\)', content)
            for match in pattern2:
                file_tags.append(match.lower())
                
            # Pattern 3: AddMaxValueTag with parameters: special case for parameterized max value tags
            pattern3 = re.findall(r'AddMaxValueTag\([^)]+\)', content)
            for match in pattern3:
                file_tags.append('maxvalue')
            
            # merging {max} from darkforest with maxvalue from circomlib as they represent same constraint
            normalized_tags = []
            for tag in file_tags:
                if tag == 'max':
                    normalized_tags.append('maxvalue')
                else:
                    normalized_tags.append(tag)
            
            # adding tags to appropriate project and overall list
            if project in tags_by_project:
                tags_by_project[project].extend(normalized_tags)
            all_tags.extend(normalized_tags)
                
        except Exception as e:
            print(f"Error reading {file_path}: {e}")
    
    return all_tags, tags_by_project

# Analyzes and counts tag frequency using Counter.
def analyze_tags(tags):
    
    # counting all tags for frequency analysis
    tag_counts = Counter(tags)
    
    return tag_counts

# Writes comprehensive tag analysis results to a file including project comparisons and statistics.
def write_results_to_file(all_tags, tags_by_project, total_files, output_file="tag_analysis_results.txt"):
    
    with open(output_file, 'w') as f:
        f.write("SIGNAL TAGS ANALYSIS RESULTS\n")
        f.write("=" * 80 + "\n")
        f.write(f"Analysis Date: {__import__('datetime').datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write(f"Total files analyzed: {total_files} .circom files\n\n")
        
        # calculating overall statistics for summary
        overall_counts = analyze_tags(all_tags)
        total_tags = len(all_tags)
        
        f.write("\n" + "=" * 80 + "\n")
        f.write("OVERALL STATISTICS\n")
        f.write("=" * 80 + "\n")
        f.write(f"Total tag instances: {total_tags}\n")
        f.write(f"Unique tag types: {len(overall_counts)}\n\n")
        
        # writing overall most common tags with percentages
        f.write("MOST COMMON TAGS (OVERALL):\n")
        f.write("-" * 40 + "\n")
        for tag, count in overall_counts.most_common():
            percentage = (count / total_tags) * 100
            f.write(f"{tag:20} : {count:4} ({percentage:5.1f}%)\n")
        
        f.write("\n" + "=" * 80 + "\n")
        f.write("ANALYSIS BY PROJECT\n")
        f.write("=" * 80 + "\n")
        
        # mapping project keys to descriptive names for output
        project_names = {
            'circomlib': 'Circomlib (circomlib-only_adding_tags)',
            'ecdsa': 'ECDSA (circom-ecdsa-master)', 
            'darkforest': 'Dark Forest (darkforest-with-tags-main)'
        }
        
        for project_key, project_name in project_names.items():
            project_tags = tags_by_project.get(project_key, [])
            if not project_tags:
                continue
                
            project_counts = analyze_tags(project_tags)
            project_total = len(project_tags)
            
            f.write(f"\n{project_name}\n")
            f.write("-" * len(project_name) + "\n")
            f.write(f"Tag instances: {project_total} ({(project_total/total_tags)*100:.1f}% of all tags)\n")
            f.write(f"Unique tag types: {len(project_counts)}\n\n")
            
            # writing project tags with both project and overall percentages for context
            f.write("Most Common Tags:\n")
            for tag, count in project_counts.most_common():
                project_percentage = (count / project_total) * 100
                overall_percentage = (count / total_tags) * 100
                f.write(f"  {tag:18} : {count:3} ({project_percentage:5.1f}% of project, {overall_percentage:4.1f}% of total)\n")
        
        # creating comprehensive comparison table across all projects
        f.write("\n" + "=" * 80 + "\n")
        f.write("PROJECT COMPARISON TABLE\n")
        f.write("=" * 80 + "\n")
        
        # getting all unique tags across projects for complete comparison
        all_unique_tags = set()
        for project_tags in tags_by_project.values():
            all_unique_tags.update(Counter(project_tags).keys())
        
        # writing header for comparison table
        f.write(f"{'Tag':<15} | {'Circomlib':<16} | {'ECDSA':<10} | {'DarkForest':<14} | {'Total':<5}\n")
        f.write("-" * 75 + "\n")
        
        # writing each tag's distribution across projects with percentages
        for tag in sorted(all_unique_tags):
            circomlib_count = Counter(tags_by_project['circomlib']).get(tag, 0)
            ecdsa_count = Counter(tags_by_project['ecdsa']).get(tag, 0)
            darkforest_count = Counter(tags_by_project['darkforest']).get(tag, 0)
            total_count = circomlib_count + ecdsa_count + darkforest_count
            
            circomlib_pct = (circomlib_count/len(tags_by_project['circomlib'])*100) if tags_by_project['circomlib'] else 0
            ecdsa_pct = (ecdsa_count/len(tags_by_project['ecdsa'])*100) if tags_by_project['ecdsa'] else 0
            darkforest_pct = (darkforest_count/len(tags_by_project['darkforest'])*100) if tags_by_project['darkforest'] else 0
            
            circomlib_str = f"{circomlib_count:>2} ({circomlib_pct:4.1f}%)"
            ecdsa_str = f"{ecdsa_count:>2} ({ecdsa_pct:4.1f}%)"
            darkforest_str = f"{darkforest_count:>2} ({darkforest_pct:4.1f}%)"
            
            f.write(f"{tag:<15} | {circomlib_str:<16} | {ecdsa_str:<10} | {darkforest_str:<14} | {total_count:>5}\n")
        
        # writing project totals for summary
        f.write("-" * 75 + "\n")
        circomlib_total = len(tags_by_project['circomlib'])
        ecdsa_total = len(tags_by_project['ecdsa'])
        darkforest_total = len(tags_by_project['darkforest'])
        f.write(f"{'TOTAL':<15} | {circomlib_total:>16} | {ecdsa_total:>10} | {darkforest_total:>14} | {total_tags:>5}\n")
    
    print(f"\nResults written to: {output_file}")
    return output_file

def main():
    search_dir = "../../evaluation/tagged-programs"
    
    print("Analyzing signal tags in tagged-programs folder...")
    print("=" * 60)
    
    # extracting tags from all circom files in the directory
    all_tags, tags_by_project = extract_tags_from_files(search_dir)
    
    # counting total files for the report
    total_files = len(glob.glob(os.path.join(search_dir, "**/*.circom"), recursive=True))
    
    # writing detailed results to file for comprehensive analysis
    output_file = write_results_to_file(all_tags, tags_by_project, total_files)
    
    print(f"\nAnalysis complete. Detailed results written to: {output_file}")

if __name__ == "__main__":
    main()
