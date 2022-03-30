

# See http://numbers.computation.free.fr/Constants/Miscellaneous/zetaevaluations.pdf (Proposition 1)

from mpmath import mp

mp.dps = 50

def d(n, k):
    return (-1) ** (k-1) * n * sum(map(
        lambda j: mp.factorial(n+j-1) * (4 ** j) / (mp.factorial(n-j) * mp.factorial(2*j)),
        range(k, n+1)
    ))

n = 18
s = 0

z = 2

for i in range(1, n+1):
    print(d(n, i))

    s += d(n, i) / (i ** z)

s *= 1 / (-d(n, 0) * (1 - 2**(1-z)))
print(d(n, 0))
