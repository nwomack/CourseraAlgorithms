from functools import reduce


def removezeros(l: list) -> list:
    for i, v in enumerate(l):
        if v != 0:
            return l[i:]
    return []


def _add(x: list, y: list) -> list:
    lx = len(x)
    ly = len(y)
    m = max(lx, ly)
    assert m > 0
    z = []
    carry = 0
    for i in range(m):
        j = -1 - i
        z = [carry] + z
        carry = 0
        if lx > i and ly > i:
            z[j] += x[j] + y[j]
        else:
            if lx > i:
                z[j] += x[j]
            else:
                z[j] += y[j]
        if z[j] > 9:
            carry = int(z[j] / 10)
            z[j] %= 10
    if carry > 0:
        z = [carry] + z
    return z


def add(*args):
    return reduce(_add, args)


def _sub(x: list, y: list) -> list:
    lx = len(x)
    ly = len(y)
    z = []
    for i in range(ly):
        j = -1 - i
        if y[j] > x[j]:
            x[j-1] = x[j-1] - 1
            x[j] += 10
        z = [x[j] - y[j]] + z
    for i in range(ly, lx):
        j = -1 -i
        z = [x[j]] + z
    return removezeros(z)


def sub(*args):
    return reduce(_sub, args)


def mul(x: list, y: list):
    z = x[0] * y[0]
    if z > 9:
        c = int(z / 10)
        z %= 10
        return [c, z]
    else:
        return [] if z == 0 else [z]


def mulr(x: list, y: list):
    if len(x) == 0 or len(y) == 0:
        return []
    if len(x) == 1 and len(y) == 1:
        return mul(x, y)

    n = max(len(x), len(y))
    n = n + 1 if n % 2 else n
    if (len(x) < n):
        x = [0] * (n - len(x)) + x
    if (len(y) < n):
        y = [0] * (n - len(y)) + y
    n_2 = int(n / 2)

    a = removezeros(x[0:n_2])
    b = removezeros(x[n_2:n])
    c = removezeros(y[0:n_2])
    d = removezeros(y[n_2:n])

    ac = mulr(a, c)
    ad = mulr(a, d)
    bc = mulr(b, c)
    bd = mulr(b, d)

    ac_shift = ac + [0] * n
    ad_shift = ad + [0] * n_2
    bc_shift = bc + [0] * n_2

    result = add(ac_shift, ad_shift, bc_shift, bd)
    return result


def kura(x: list, y: list):
    if len(x) == 0 or len(y) == 0:
        return []
    if len(x) == 1 and len(y) == 1:
        return mul(x, y)

    n = max(len(x), len(y))
    n = n + 1 if n % 2 else n
    if (len(x) < n):
        x = [0] * (n - len(x)) + x
    if (len(y) < n):
        y = [0] * (n - len(y)) + y
    n_2 = int(n / 2)

    a = removezeros(x[0:n_2])
    b = removezeros(x[n_2:n])
    c = removezeros(y[0:n_2])
    d = removezeros(y[n_2:n])

    ac = kura(a, c)
    bd = kura(b, d)
    abcd = kura(add(a, b), add(c, d))
    ad_bc = sub(abcd, ac, bd)

    ac_shift = ac + [0] * n
    ad_bc_shift = ad_bc + [0] * n_2

    result = add(ac_shift, ad_bc_shift, bd)
    return result


def testsub(x:int, y:int) -> None:
    xl = [int(d) for d in str(x)]
    yl = [int(d) for d in str(y)]
    zl = sub(xl, yl)
    z = 0 if zl == [] else int(''.join(str(i) for i in zl))
    assert z == x - y, "expected %d - %d = %d, got %d" % (x, y, (x - y), z)


def testadd(x:int, y:int) -> None:
    xl = [int(d) for d in str(x)]
    yl = [int(d) for d in str(y)]
    zl = add(xl, yl)
    z = int(''.join(str(i) for i in zl))
    assert z == x + y, "expected %d + %d = %d, got %d" % (x, y, (x + y), z)


def testmulr(x:int, y:int) -> None:
    xl = [int(d) for d in str(x)]
    yl = [int(d) for d in str(y)]
    zl = mulr(xl, yl)
    z = int(''.join(str(i) for i in zl))
    assert z == x * y, "expected %d" % (x * y)


def testkura(x:int, y:int) -> None:
    xl = [int(d) for d in str(x)]
    yl = [int(d) for d in str(y)]
    zl = kura(xl, yl)
    z = int(''.join(str(i) for i in zl))
    assert z == x * y, "expected %d" % (x * y)


testsub(5, 4)
testsub(5, 0)
testsub(10, 1)
testsub(1000, 999)
testsub(1001, 900)
testsub(100, 100)

testadd(1, 2)
testadd(10, 2)
testadd(100, 2)
testadd(100, 20)
testadd(101, 201)
testadd(111, 211)
testadd(999, 999)
testadd(9999, 999)
testadd(999987, 10001)

testmulr(1, 2)
testmulr(9, 9)
testmulr(12, 34)
testmulr(10, 100)
testmulr(99, 99)
testmulr(100, 100)
testmulr(101, 101)
testmulr(10100, 10122)
testmulr(10101, 10122)
testmulr(10102, 10122)
testmulr(10113, 99122)

testkura(1, 2)
testkura(9, 9)
testkura(12, 34)
testkura(10, 100)
testkura(99, 99)
testkura(100, 100)
testkura(101, 101)
testkura(10100, 10122)
testkura(10101, 10122)
testkura(10102, 10122)
testkura(10113, 99122)

x = 3141592653589793238462643383279502884197169399375105820974944592
y = 2718281828459045235360287471352662497757247093699959574966967627
testmulr(x, y)
testkura(x, y)

