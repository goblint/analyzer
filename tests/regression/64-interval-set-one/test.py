import os
for index, filename in enumerate(os.listdir(".")):
    if ".c" in filename:
        new_filename = f"{index}{filename[2:]}"
        #print(new_filename)
        os.rename(filename, new_filename)