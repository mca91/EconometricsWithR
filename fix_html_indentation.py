#!/usr/bin/env python3
"""
Script to remove code indentation from HTML files in the EconometricsWithR project.
This script processes all .html files and removes leading whitespace from code blocks
within <code> tags while preserving the HTML structure.
"""

import os
import re
import glob
from pathlib import Path


def remove_code_indentation(content):
    """
    Remove indentation from code blocks within <code> tags.
    """

    def process_code_block(match):
        opening_tag = match.group(1)  # The opening <code...> tag
        code_content = match.group(2)  # The content between tags
        closing_tag = match.group(3)  # The closing </code> tag

        # Split content into lines
        lines = code_content.split("\n")
        processed_lines = []

        for line in lines:
            # Remove leading whitespace from each line
            # But preserve empty lines as they are
            if line.strip() == "":
                processed_lines.append("")
            else:
                processed_lines.append(line.lstrip())

        # Join the lines back together
        processed_content = "\n".join(processed_lines)

        return opening_tag + processed_content + closing_tag

    # Pattern to match <code ...> content </code> blocks
    # This handles multi-line code blocks with any attributes in the opening tag
    pattern = r"(<code[^>]*>)(.*?)(</code>)"

    # Process all code blocks, using DOTALL flag to match newlines
    result = re.sub(pattern, process_code_block, content, flags=re.DOTALL)

    return result


def process_html_file(file_path):
    """
    Process a single HTML file to remove code indentation.
    """
    try:
        # Read the file
        with open(file_path, "r", encoding="utf-8") as f:
            content = f.read()

        # Process the content
        processed_content = remove_code_indentation(content)

        # Write back the processed content
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(processed_content)

        print(f"Processed: {file_path}")

    except Exception as e:
        print(f"Error processing {file_path}: {e}")


def main():
    """
    Main function to process all HTML files in the workspace.
    """
    # Get the current directory (should be the EconometricsWithR directory)
    workspace_dir = Path("/Users/martin/git_projects/EconometricsWithR/DCL")

    # Find all HTML files
    html_files = []
    for pattern in ["**/*.html"]:
        html_files.extend(glob.glob(str(workspace_dir / pattern), recursive=True))

    print(f"Found {len(html_files)} HTML files to process")

    # Process each file
    for file_path in html_files:
        process_html_file(file_path)

    print("All HTML files have been processed!")


if __name__ == "__main__":
    main()
