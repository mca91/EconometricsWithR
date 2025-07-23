#!/usr/bin/env python3
"""
Improved script to remove ALL indentation from HTML files.
This script processes all .html files and removes leading whitespace (spaces and tabs)
from every line, including HTML tags and code blocks.
"""

import os
import re
import glob
from pathlib import Path


def remove_all_indentation(content):
    """
    Remove ALL indentation (spaces and tabs) from every line in the HTML file.
    This includes HTML tags, code blocks, and all other content.
    """
    # Split content into lines
    lines = content.split("\n")
    processed_lines = []

    for line in lines:
        # Remove ALL leading whitespace (spaces and tabs) from each line
        # But preserve completely empty lines as they are
        if line.strip() == "":
            processed_lines.append("")
        else:
            # Use lstrip() to remove all leading whitespace (spaces, tabs, etc.)
            processed_lines.append(line.lstrip())

    # Join the lines back together
    return "\n".join(processed_lines)


def process_html_file(file_path):
    """
    Process a single HTML file to remove all indentation.
    """
    try:
        # Read the file
        with open(file_path, "r", encoding="utf-8") as f:
            content = f.read()

        # Process the content
        processed_content = remove_all_indentation(content)

        # Only write if content changed
        if processed_content != content:
            with open(file_path, "w", encoding="utf-8") as f:
                f.write(processed_content)
            print(f"Processed: {file_path}")
        else:
            print(f"No changes needed: {file_path}")

    except Exception as e:
        print(f"Error processing {file_path}: {e}")


def main():
    """
    Main function to process all HTML files in the workspace.
    """
    # Get the current directory (should be the EconometricsWithR directory)
    workspace_dir = Path("/Users/martin/git_projects/EconometricsWithR")

    # Find all HTML files recursively
    html_files = []
    for pattern in ["**/*.html"]:
        html_files.extend(glob.glob(str(workspace_dir / pattern), recursive=True))

    print(f"Found {len(html_files)} HTML files to check")
    print("Removing all indentation from HTML files...\n")

    # Process each file
    processed_count = 0
    for file_path in html_files:
        old_count = processed_count
        process_html_file(file_path)
        # This is a simple way to count if anything was printed with "Processed:"

    print(f"\nCompleted processing all HTML files!")


if __name__ == "__main__":
    main()
