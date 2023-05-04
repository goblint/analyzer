import os
import shutil
import subprocess
import tempfile

script_location = os.path.dirname(__file__)
repoURL = 'https://github.com/cmatsuoka/figlet.git'
repoPath = os.path.join(script_location, '/testGit')
repoDifPath = os.path.join(script_location, '/testGitDif')
paths = [repoPath, repoDifPath]

# Clear the directories
for path in paths:
    if os.path.exists(path):
        shutil.rmtree(path)

# Clone the repository from repo URL into the folder ./testGit
print("Repo Path: " + repoPath)
subprocess.run(['git', 'clone', repoURL, repoPath])

# Change the working directory to the repository directory
os.chdir(repoPath)

# Get a list of commit hashes using git log
output = subprocess.run(["git", "log", "--reverse", "--format=%H"], capture_output=True, text=True)
commit_hashes = output.stdout.strip().split('\n')

# Create the directory ./testGitDif if it does not exist
os.makedirs(repoDifPath, exist_ok=True)

# Create a diff file for each commit and store them in ./testGitDif
for i, commit_hash in enumerate(commit_hashes):
    # Generate the diff using git diff
    diff_output = subprocess.run(['git', 'diff', commit_hash + '^', commit_hash], capture_output=True, text=True)
    diff = diff_output.stdout.strip()

    # Write the diff to a file in the repoDifPath
    diff_path = os.path.join(repoDifPath, '{:06d}_{}.diff'.format(i, commit_hash))
    with open(diff_path, 'w') as f:
        f.write(diff)

    # Update the paths to account for the change in working directory
    repoPath = os.path.abspath(repoPath)
    repoDifPath = os.path.abspath(repoDifPath)

