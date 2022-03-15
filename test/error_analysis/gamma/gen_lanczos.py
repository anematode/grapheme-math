from mpmath import mp
from math import comb
import functools

# p_k(g) = sqrt(2) / pi * sum (l = 0 to k) (C(2k+1, 2l+1))(l - 1/2)!(l+g+1/2)^(-l-1/2) exp(l + g + 1/2)

mp.dps = 100

@functools.cache
def C(n, m):
    if n == 1 and m == 1:
         return mp.mpf(1)
    if n == 2 and m == 2:
         return mp.mpf(1)
    if m == 1 and n > 1:
         return -C(n-2, m)
    if m == n and n > 1:
         return 2 * C(n-1, n-1)
    return 2 * C(n-1, m-1) - C(n-2, m)

# See http://www.numericana.com/answer/info/godfrey.htm
def pk(g, k):
    s = mp.mpf(0)
    for l in range(0, k + 1):
        s += C(2 * k + 1, 2 * l + 1) * mp.factorial(l - 0.5) * (l + g + 0.5) ** (-(l + 0.5)) * mp.exp(l + g + 0.5)

    return mp.sqrt(2) / mp.pi * s

def fg(g,l):
    return mp.sqrt(2) / mp.pi * mp.factorial(l - 0.5) * (l + g + 0.5) ** (-(l + 0.5)) * mp.exp(l + g + 0.5)

def mmult(m1, m2, dim):
    return [[sum(map(lambda k: m1[j][k] * m2[k][i], range(dim))) for i in range(dim)] for j in range(dim)]

def d(k):
    # diagonal matrix
    return [[0 if i != j else mp.mpf(1) if i == -1 else -mp.factorial(2 * i + 2) / (2 * mp.factorial(i) * mp.factorial(i+1)) for i in range(-1, k-1)] for j in range(-1, k-1)]

def b(k):
    return [[1 if j == -1 else 0 if j > i else 1 if j == i else comb(i+j+1, 2*j+1) * (-1)**(i + j) for i in range(-1, k - 1)] for j in range(-1, k - 1)]

def c(g, k):
    cm = list([[0 if i > j else C(2 * j + 1, 2 * i + 1) for i in range(k)] for j in range(k)])
    cm[0][0] = mp.mpf(0.5)
    return cm

def dbc(g, k):
    return mmult(d(k), mmult(b(k), c(g, k), k), k)

def coeffs(g, k):
    mm = dbc(g, k)
    pks = [fg(g, i) for i in range(0, k)]

    return [ sum(mm[j][i] * pks[i] for i in range(k)) for j in range(k) ]

def mpf_to_dd(f):
    rounded = float(f)
    err = float(f - rounded)

    return (str(rounded) + "," + str(err))

g = 14
k = 18

print(','.join(list(map(mpf_to_dd, coeffs(g,k)))))
