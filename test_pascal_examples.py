#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import subprocess
import glob
import sys


def compile_and_run_pascal_tests():
    """
    Compile and run all Pascal examples in the pascal_examples directory.
    Clean up generated files after testing.
    """
    # Change to the pascal_examples directory
    script_dir = os.path.dirname(os.path.abspath(__file__))
    pascal_dir = os.path.join(script_dir, "pascal_examples")

    if not os.path.exists(pascal_dir):
        print(f"Error: Directory {pascal_dir} does not exist")
        return False

    os.chdir(pascal_dir)

    # Find all .pas files
    pas_files = sorted(glob.glob("*.pas"))

    if not pas_files:
        print("No .pas files found in the directory")
        return False

    print(f"Found {len(pas_files)} Pascal files to test:")
    for pas_file in pas_files:
        print(f"  - {pas_file}")

    passed = 0
    failed = 0

    # Compile and run each file
    for pas_file in pas_files:
        print(f"\nTesting {pas_file}...")

        # Compile the Pascal file
        try:
            compile_result = subprocess.run(
                ["fpc", pas_file], capture_output=True, text=True, timeout=30
            )

            if compile_result.returncode != 0:
                print(f"  COMPILATION FAILED for {pas_file}")
                print(f"  Error: {compile_result.stderr}")
                failed += 1
                continue

        except subprocess.TimeoutExpired:
            print(f"  COMPILATION TIMEOUT for {pas_file}")
            failed += 1
            continue
        except FileNotFoundError:
            print("Error: fpc compiler not found. Please install Free Pascal Compiler.")
            return False

        # Get the executable name (usually same as source but with .exe extension on Windows)
        exe_file = os.path.splitext(pas_file)[0] + ".exe"
        if not os.path.exists(exe_file):
            # On some systems, the executable might not have .exe extension
            exe_file = os.path.splitext(pas_file)[0]

        if not os.path.exists(exe_file):
            print(f"  EXECUTABLE NOT FOUND for {pas_file}")
            failed += 1
            continue

        # Run the executable
        try:
            run_result = subprocess.run(
                [exe_file], capture_output=True, text=True, timeout=10
            )

            if run_result.returncode == 0:
                print(f"  PASSED: {pas_file}")
                passed += 1
                # Print output if there is any
                if run_result.stdout.strip():
                    print(f"    Output: {run_result.stdout.strip()}")
            else:
                print(f"  FAILED: {pas_file}")
                print(f"    Return code: {run_result.returncode}")
                if run_result.stderr.strip():
                    print(f"    Error: {run_result.stderr.strip()}")
                failed += 1

        except subprocess.TimeoutExpired:
            print(f"  RUN TIMEOUT for {pas_file}")
            failed += 1
        except Exception as e:
            print(f"  RUN ERROR for {pas_file}: {e}")
            failed += 1

    # Clean up generated files
    print("\nCleaning up generated files...")
    patterns_to_delete = ["*.o", "*.exe", "*.ppu", "*.bak"]
    deleted_count = 0

    for pattern in patterns_to_delete:
        files = glob.glob(pattern)
        for file in files:
            try:
                os.remove(file)
                deleted_count += 1
            except Exception as e:
                print(f"  Warning: Could not delete {file}: {e}")

    # Print summary
    print("\n=== TEST SUMMARY ===")
    print(f"Total files tested: {len(pas_files)}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    print(f"Files cleaned up: {deleted_count}")

    if failed == 0:
        print("All tests PASSED!")
        return True
    else:
        print("Some tests FAILED!")
        return False


if __name__ == "__main__":
    success = compile_and_run_pascal_tests()
    sys.exit(0 if success else 1)
