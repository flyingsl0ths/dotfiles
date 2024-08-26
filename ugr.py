import os
import subprocess


def run_git_pull(directory: str):
    """
    Runs 'git pull' in the specified directory if it is a Git repository.
    """
    try:
        # Check if the directory is a Git repository by looking for a .git folder
        if os.path.isdir(os.path.join(directory, ".git")):
            print(f"Running 'git pull' in {directory}...")
            # Run the 'git pull' command
            result = subprocess.run(
                ["git", "pull"], cwd=directory, capture_output=True, text=True
            )
            # Print the output and error of the command
            print(result.stdout)
            if result.stderr:
                print(f"Error: {result.stderr}")
        else:
            print(f"{directory} is not a Git repository.")
    except Exception as e:
        print(f"An error occurred while processing {directory}: {e}")


def iterate_and_pull(root_directory: str):
    """
    Iterates through all top-level directories starting from root_directory
    and runs 'git pull' in each Git repository.
    """
    # Get a list of all entries in the root directory
    for entry in os.listdir(root_directory):
        entry_path = os.path.join(root_directory, entry)
        # Check if the entry is a directory
        if os.path.isdir(entry_path):
            run_git_pull(entry_path)


if __name__ == "__main__":
    root_directory: str = os.getcwd()
    iterate_and_pull(root_directory)
