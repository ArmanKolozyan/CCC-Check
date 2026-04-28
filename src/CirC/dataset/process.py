# Python script to process projects from the dataset
import argparse
import csv
import os
import glob
import sys
import subprocess
from collections import defaultdict

# Some constants that we are going to use
CUR_DIR = os.path.dirname(os.path.abspath(__file__))
DSL_DIR = {
    "zok": os.path.join(CUR_DIR, "zokrates", "macro"),
    "zokc": os.path.join(CUR_DIR, "zokrates_curly", "macro"),
    "circom": os.path.join(CUR_DIR, "circom", "macro"),
}
DSL_LANG = {
    "zok": "zsharp",
    "zokc": "zsharp-curly",
}
CIRC_BIN = os.path.join(CUR_DIR, "..", "target", "release", "examples", "circ")
COMPILE_ARGS = ["nothing"]
DATAN_ARGS = ["datan"]

def check_projects(dataset, verbose):
    """Perform various checks on the CSV and the projects

    1. Check if every line in the CSV is well-formed
    2. Check if the directory for each project exists
    3. Check if each project has the defined files
    """
    first_row = dataset[0]
    # Results is a list of tuples (counter, row, files)
    results = []
    for counter, row in enumerate(dataset):
        # Add 2 to the counter to account for the header row
        counter = counter + 2
        # Check 1
        if len(row) != len(first_row):
            print(f"Error: The row {counter} has {len(row)} columns, but the header row has {len(first_row)} columns")
            if verbose:
                print(f"Row: {row}")
            sys.exit(1)
        # Check if the project directory exists
        dsl, repo_url, commit, directory, file = row[0], row[1], row[2], row[3], row[4]
        # If the commit is empty then we are processing a tutorial or blog post
        # so the project directory is the directory row
        if commit == "":
            project_dir = os.path.join(DSL_DIR[dsl], directory)
        else:
            if repo_url.startswith("https://github.com/"):
                github_len = len("https://github.com/")
                project_dir = os.path.join(DSL_DIR[dsl], repo_url[github_len:])
            else:
                print(f"Error: The repository URL {repo_url} is not a valid GitHub URL")
                sys.exit(1)
        # Check 2
        if not os.path.exists(project_dir):
            print(f"Error: The project directory {project_dir} does not exist (row {counter})")
            sys.exit(1)
        # If the commit is empty, then the directory is the same as the project directory
        if commit == "":
            directories = [project_dir]
        else:
            # There are three cases:
            # A. The directory is './'
            if directory == "./":
                directories = [project_dir]
            # B. The directory is a relative path from the project directory
            #    and does not contain either '*' or '{...}'
            elif not "*" in directory and not "{" in directory:
                directories = [os.path.join(project_dir, directory)]
            # C. The directory is a relative path from the project directory
            #    and contains '*'. In this case, we need to expand the directory
            elif "*" in directory:
                tmp_dir = os.path.join(project_dir, directory)
                directories = {path for path in glob.glob(tmp_dir) if os.path.isdir(path)}
                tmp_dirs_len = len(directories)
                while True:
                    tmp_dirs = list(directories)
                    for dir in tmp_dirs:
                        directories.update(
                            {os.path.join(dir, path) 
                             for path in os.listdir(dir) 
                             if os.path.isdir(os.path.join(dir, path))})
                    if len(directories) == tmp_dirs_len:
                        break
                    tmp_dirs_len = len(directories)
            else:
                print(f"Error: The directory {directory} is not a valid directory (row {counter})")
                sys.exit(1)
        # In each directory, we need to find all the files (or file) that match the file pattern
        files = []
        for directory in directories:
            if "*" in file:
                files.extend(glob.glob(os.path.join(directory, file)))
            elif "{" in file:
                print("Error: Curly braces are not supported in file (row {counter})")
                sys.exit(1)
            else:
                files.append(os.path.join(directory, file))
        # Check 3
        if not files:
            print(f"Error: No files found in the project directory {project_dir} (row {counter})")
            sys.exit(1)
        # Check if the file exists
        for file in files:
            if not os.path.exists(file):
                print(f"Error: The file {file} does not exist in the project directory {project_dir} (row {counter})")
                sys.exit(1)
        results.append((counter, row, files))
        
    print("All rows and projects are well-formed")
    return results

def process_projects(dataset, verbose, mode, timeout=60):
    """Process all files, report the success rate, and return processed files"""
    if mode == "compile":
        args = COMPILE_ARGS
    elif mode == "datan":
        args = DATAN_ARGS
    else:
        print(f"Error: Invalid mode {mode} (valid modes: compile, datan)")
        sys.exit(1)
    stats = defaultdict(lambda: {
        "projects": {"total": 0, "success": 0, "failed": 0}, 
        "files": {"total": 0, "success": 0, "failed": 0}
    })
    # Results is a list of tuples (counter, row, files, stdout)
    # where files are the files that were successfully processed
    results = []
    tmp_counter = 1
    for counter, row, files in dataset:
        print(f"Compiling project {counter} (row) -- {tmp_counter} of {len(dataset)}")
        if verbose:
            print(f"Row: {row}")
        tmp_counter += 1
        tmp_dsl = row[0]
        stats[tmp_dsl]["projects"]["total"] += 1
        tmp_project_status = True
        tmp_files = []
        for file in files:
            stats[tmp_dsl]["files"]["total"] += 1
            # TODO: Fix me, use circ for circom
            if tmp_dsl == "circom":
                CMD = ["circom", file]
            else:
                CMD = [CIRC_BIN, file, "--language", DSL_LANG[tmp_dsl]] + args
            if verbose:
                print(f"Running command: {' '.join(CMD)}")
            # Run the command
            try:
                result = subprocess.run(CMD, capture_output=True, text=True, timeout=timeout)
            except subprocess.TimeoutExpired:
                tmp_project_status = False
                stats[tmp_dsl]["files"]["failed"] += 1
                if verbose:
                    print(f"Error: The file {file} timed out (row {counter})")
                continue
            if result.returncode != 0:
                tmp_project_status = False
                stats[tmp_dsl]["files"]["failed"] += 1
                if verbose:
                    print(f"Error: The file {file} failed to get processed (row {counter})")
                    print(result.stdout)
                    print(result.stderr)
            else:
                tmp_files.append(file)
                results.append((counter, row, file, result.stdout))
                stats[tmp_dsl]["files"]["success"] += 1
        if tmp_project_status:
            stats[tmp_dsl]["projects"]["success"] += 1
        else:
            stats[tmp_dsl]["projects"]["failed"] += 1
    for dsl, result in stats.items():
        print(f"{dsl} DSL:")
        print(f"  Projects: {result['projects']['success']}/{result['projects']['total']} successful")
        print(f"  Files: {result['files']['success']}/{result['files']['total']} successful")
    return results


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", choices=["check", "compile", "datan"], required=True)
    parser.add_argument("--dsl", choices=["zok", "zokc", "circom"])
    parser.add_argument("--dataset", required=True)
    parser.add_argument("--verbose", action="store_true")
    args = parser.parse_args()

    # Checks if the dataset file exists
    if not os.path.exists(args.dataset):
        print(f"Error: The dataset file {args.dataset} does not exist")
        return

    # Read the dataset file
    with open(args.dataset, "r") as f:
        reader = csv.reader(f)
        header = next(reader)
        if args.dsl:
            dataset = [row for row in reader if row[0] == args.dsl]
        else:
            dataset = [row for row in reader]

    if args.dsl:
        print(f"Processing {args.dsl} DSL")
    else:
        print("Processing all DSLs")

    if args.mode == "check":
        check_projects(dataset, args.verbose)
    elif args.mode == "compile":
        # First check if the binary exists
        #if not os.path.exists(CIRC_BIN):
        #    print(f"Error: The binary {CIRC_BIN} does not exist. Please build circ first.")
        #    sys.exit(1)
        dataset = check_projects(dataset, args.verbose)
        dataset = process_projects(dataset, args.verbose, "compile")
    elif args.mode == "datan":
        dataset = check_projects(dataset, args.verbose)
        dataset = process_projects(dataset, args.verbose, "datan")
        print(f"Save datan results to tmp/datan")
        for _, _, file, stdout in dataset:
            file_path = '/'.join(['tmp', 'datan', file.replace(CUR_DIR, '')]) + ".dl"
            dir_path = os.path.dirname(file_path)
            # Safely create all the directories
            os.makedirs(dir_path, exist_ok=True)
            # Write the stdout to the file
            with open(file_path, "w") as f:
                f.write(stdout[61:])
    else:
        print(f"Error: Invalid mode {args.mode} (valid modes: check, compile, datan)")
        sys.exit(1)

if __name__ == "__main__":
    main()
