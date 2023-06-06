import os
import argparse
import subprocess
import sys

BUILD_CMAKE = 'cmake'
BUILD_MAKE = 'make'

def cil_transform(goblint_path, project_dir, output_dir, build_system):
    goblint_path = os.path.abspath(goblint_path)
    project_dir = os.path.abspath(project_dir)
    output_dir = os.path.abspath(output_dir)

    # Check if Makefile/CMakeLists.txt exists in the directory
    build_file = 'Makefile' if build_system == BUILD_MAKE else 'CMakeLists.txt'
    if not os.path.isfile(os.path.join(project_dir, build_file)):
        print(f"No {build_file} found in the project directory: {project_dir}")
        return
    
    # Navigate to the directory
    original_dir = os.getcwd()
    os.chdir(project_dir)

    # Run make/cmake command
    if build_system == BUILD_MAKE:
        build_process = subprocess.run([build_system, 'DCMAKE_EXPORT_COMPILE_COMMANDS=ON'])
    elif build_system == BUILD_CMAKE:
        build_process = subprocess.run(['bear', '--', 'make'])
    if build_process.returncode != 0:
        print(f"Error in {build_system} command:")
        print(build_process.stderr.decode())
        return

    # Run goblint with cil option
    goblint_process = subprocess.run([goblint_path, '--set', 'justcil', 'true', '--set', 'cil.merge.inlines', 'false', project_dir], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if goblint_process.returncode != 0:
        print("Error in goblint command:")
        print(goblint_process.stderr.decode())
        return

    # Output the transformed CIL code
    with open(os.path.join(output_dir, 'cil.c'), 'w') as f:
        f.write(goblint_process.stdout.decode())
    print("CIL transformation completed. Transformed code written to cil.c")

    # Navigate back to the original directory
    os.chdir(original_dir)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Transform a project into a single CIL file using Goblint.')
    parser.add_argument('goblint_path', type=str, help='Path to the goblint binary')
    parser.add_argument('project_dir', type=str, help='Path to the project directory')
    parser.add_argument('output_dir', type=str, help='Path to the output directory')
    parser.add_argument('--build', choices=[BUILD_MAKE, BUILD_CMAKE], required=True, help=f'Build system to use ({BUILD_MAKE} or {BUILD_CMAKE})')
    
    args = parser.parse_args()
    cil_transform(args.goblint_path, args.project_dir, args.output_dir, args.build)
