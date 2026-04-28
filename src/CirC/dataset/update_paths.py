"""
This script updates the paths of circom circuits to the correct circomlib
"""

import os
import shutil
import sys
import glob
# we should get the path of the project from cli
project_path = sys.argv[1]

# Get the path of the current file
current_path = os.path.dirname(os.path.abspath(__file__))

# Get the path of the circomlib
circomlib_path = os.path.join(current_path, "circom", "node_modules", "circomlib")

# Get all the circom files in the project path and its subdirectories
circom_files = glob.glob(os.path.join(project_path, "**/*.circom"), recursive=True)

# Let's update the paths of the circom files
print(circom_files)
for file in circom_files:
    lines = []
    with open(file, "r") as f:
        # We need to read the file line by line
        lines = f.readlines()
        for i, line in enumerate(lines):
            # We need to update the path of the circomlib to the correct path
            # for example:
            # include "../../../node_modules/circomlib/circuits/comparators.circom";
            # should be updated to:
            # include "../../../../../node_modules/circomlib/circuits/comparators.circom";
            # to get how many ../ we need we must compute how many levels deep we are in that file,
            #, i.e., the number of dirs in the path of the file
            deep_level = file.count("/") 
            prepend_str = "../" * (deep_level - 1) + "node_modules/"
            # IF the line contains circomlib/, then we need to update the path
            if "include" in line and "circomlib/" in line:
                # we need to get the path from circomlib and after.
                # For example for line:
                # include "../../../node_modules/circomlib/circuits/comparators.circom";
                # we should get :
                # "circomlib/circuits/comparators.circom";
                circomlib_path = line.split("circomlib/")[1]
                # we need to update the path
                new_line = 'include "' + prepend_str + "circomlib/" + circomlib_path
                lines[i] = new_line
            else:
                lines[i] = line
    # We need to write the updated file
    with open(file, "w") as f:
        f.writelines(lines)
