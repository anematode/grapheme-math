from mpmath import mp
import sys
import pathlib
import subprocess
from itertools import starmap
import math
import numpy as np

functions = {
    "gamma": mp.gamma
}

precision = 50
err_count = 20

def get_ulp_error(correct, returned):
    if not math.isfinite(correct) or not math.isfinite(returned):
        return 0
    return float((mp.fabs(correct - returned)) / math.ulp(returned))

def is_incorrect_overflow(correct, returned):
    correct = float(correct)

    if not math.isfinite(correct) or not math.isfinite(returned):
        if correct != returned:
            return 1

    return 0

def get_max_errors(rel_errors, count=err_count):
    rel_errors.sort(key=lambda x: x[1])
    rel_errors.reverse()

    return rel_errors[:count]

def get_overflows(rel_errors, count=err_count):
    rel_errors.sort(key=lambda x: is_incorrect_overflow(x[2], x[3]))

    return rel_errors[:count]

if __name__ == "__main__":
    folder = sys.argv[1]
    p = pathlib.Path('./' + folder)

    if not p.is_dir():
        raise Exception(f"${folder} doesn't exist or isn't a folder")

    test_value_file = p / "test_values.txt"
    test_values = None

    with open(test_value_file, 'r') as f:
        test_values = list(map(float, filter(lambda s: len(s) >= 1, f.read().split(','))))

    js_results = subprocess.check_output(["node", "analysis.js", folder, "--require esm" ]).decode()
    js_results = map(float, js_results.split('\n'))

    mp.dps = precision

    f = functions[folder]
    canonical_values = map(lambda x: f(x), test_values)

    rel_errors = list(starmap(lambda mp,js,value: (value, get_ulp_error(mp, js), mp, js), zip(canonical_values, js_results, test_values)))

    # Print relative error stats

    errs = np.array(list(starmap(lambda _0, err, _1, _2: abs(err) + 0.000001 if math.isfinite(err) else 1, rel_errors)))

    print("Max ulp errors (input, error):")

    for v, e, _0, _1 in get_max_errors(rel_errors):
        print(str(v) + ": " + str(e))

    print("Incorrect overflows (input, expected output, real output):")

    for v, e, r, _1 in get_overflows(rel_errors):
        print(str(v) + "\t" + str(e) + "\t" + str(r))

    print("Average ulp error (arithmetic): " + str(errs.mean()))
    print("Average ulp error (geometric): " + str(np.exp(np.log(errs).mean())))



