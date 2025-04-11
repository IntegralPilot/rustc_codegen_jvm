import os
import platform

# get the absolute path of the current working directory
current_directory = os.path.dirname(os.path.abspath(__file__))

# find .template files in this current directory
template_files = []
for root, dirs, files in os.walk(current_directory):
    for file in files:
        if file.endswith(".template"):
            template_files.append(os.path.join(root, file))

# copy them, removing the .template extension
# replace all instances of "../../.." with the absolute path of the current working directory
# Determine the appropriate dynamic library extension for the host platform
if platform.system() == "Windows":
    lib_extension = ".dll"
elif platform.system() == "Darwin":  # macOS
    lib_extension = ".dylib"
else:  # Assume Linux or other Unix-like systems
    lib_extension = ".so"

for template_file in template_files:
    with open(template_file, 'r') as f:
        content = f.read()
    content = content.replace("../../..", current_directory)
    content = content.replace(".dylib", lib_extension)
    new_file_path = os.path.splitext(template_file)[0]
    with open(new_file_path, 'w') as f:
        f.write(content)