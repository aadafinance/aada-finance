from sys import argv, exit
import json
import os

if len(argv) != 4:
    exit(1)

ver = argv[1]
new_ver = argv[2]
hash = argv[3]

if not os.path.isfile(new_ver):
    print("Version file not found: " + new_ver)
    exit(1)

with open(new_ver, 'r') as f:
    new_ver_json = json.load(f)

if not os.path.isfile(ver):
    ver_json = json.loads('{}')
else:
    with open(ver, 'r') as f:
        ver_json = json.load(f)

ver_json[hash] = new_ver_json[hash]
with open(ver, 'w') as f:
    f.write(json.dumps(ver_json))
