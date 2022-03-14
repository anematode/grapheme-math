from mpmath import mp
import sys
import pathlib
import subprocess
from itertools import starmap

functions = {
    "gamma": mp.gamma
}

precision = 50

def get_ulp_error(correct, returned):
    return mp.fabs(correct - returned) / correct


if __name__ == "__main__":
    folder = sys.argv[1]
    p = pathlib.Path('./' + folder)

    if not p.is_dir():
        raise Exception(f"${folder} doesn't exist or isn't a folder")

    test_value_file = p / "test_values.txt"
    test_values = None

    with open(test_value_file, 'r') as f:
        test_values = map(float, filter(lambda s: len(s) > 1 and not "VM" in s, f.read().split(',')))

    js_results = subprocess.check_output(["node", "analysis.js", folder, "--require esm" ]).decode()
    js_results = map(float, js_results.split('\n'))

    mp.dps = precision

    f = functions[folder]
    canonical_values = map(lambda x: f(x), test_values)

    rel_errors = list(starmap(lambda mp,js,value: (value, get_ulp_error(mp, js)), zip(canonical_values, js_results, test_values)))

    # Print relative error stats
    print(rel_errors)




