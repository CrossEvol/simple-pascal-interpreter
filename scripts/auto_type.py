
import argparse
import subprocess

def run_autotyping(filename):
  """
  Runs the autotyping command with the given filename.

  Args:
    filename: The path to the Python file to analyze.

  Returns:
    The output of the autotyping command.
  """

  command = [
      "python", "-m", "autotyping", 
      filename, 
      "--none-return", 
      "--scalar-return", 
      "--bool-param", 
      "--int-param", 
      "--float-param", 
      "--str-param", 
      "--bytes-param", 
      "--annotate-optional", 
      "foo:bar.Baz", 
      "--annotate-named-param", 
      "foo:bar.Baz", 
      "--guess-common-names", 
      "--annotate-magics", 
      "--annotate-imprecise-magics"
  ]

  try:
    result = subprocess.run(command, capture_output=True, text=True, check=True)
    return result.stdout
  except subprocess.CalledProcessError as e:
    print(f"Error running autotyping command: {e}")
    return e.stderr

if __name__ == "__main__":
  # Parse arguments
  parser = argparse.ArgumentParser(description="Run autotyping on a Python file")
  parser.add_argument("filename", help="Path to the Python file to analyze")
  args = parser.parse_args()

  # Run the autotyping command
  output = run_autotyping(args.filename)

  # Print the output
  print(output)
