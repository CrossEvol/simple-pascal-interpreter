#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import glob
import os
import subprocess
import sys


def run_pascal_tests():
    """
    用 dist/main.exe 解释执行所有 .pas 文件，不生成任何临时文件。
    """
    # 切到脚本所在目录
    script_dir = os.path.dirname(os.path.abspath(__file__))
    pascal_dir = os.path.join(script_dir, "pas")
    runner = os.path.join(script_dir, "dist", "main.exe")

    if not os.path.exists(pascal_dir):
        print(f"Error: Directory {pascal_dir} does not exist")
        return False

    if not os.path.isfile(runner):
        print(f"Error: Runner {runner} not found")
        return False

    os.chdir(pascal_dir)

    pas_files = sorted(glob.glob("**/*.pas", recursive=True))
    if not pas_files:
        print("No .pas files found")
        return False

    print(f"Found {len(pas_files)} Pascal files to test:")
    for f in pas_files:
        print(f"  - {f}")

    passed = failed = 0

    for pas_file in pas_files:
        print(f"\nTesting {pas_file} ...")

        try:
            # 用 dist/main.exe 直接解释执行
            result = subprocess.run(
                [runner, pas_file],
                capture_output=True,
                text=True,
                timeout=10,
                encoding="utf-8",
                errors="replace",
            )
        except subprocess.TimeoutExpired:
            print("  TIMEOUT")
            failed += 1
            continue
        except Exception as e:
            print(f"  RUN ERROR: {e}")
            failed += 1
            continue

        if result.returncode == 0:
            print("  PASSED")
            if result.stdout and result.stdout.strip():
                print(f"    Output: {result.stdout.strip()}")
            passed += 1
        else:
            print(f"  FAILED (return code {result.returncode})")
            if result.stderr and result.stderr.strip():
                print(f"    Error: {result.stderr.strip()}")
            failed += 1

    # 不再清理任何文件
    print("\n=== TEST SUMMARY ===")
    print(f"Total files tested: {len(pas_files)}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    if failed == 0:
        print("All tests PASSED!")
        return True
    else:
        print("Some tests FAILED!")
        return False


if __name__ == "__main__":
    success = run_pascal_tests()
    sys.exit(0 if success else 1)
