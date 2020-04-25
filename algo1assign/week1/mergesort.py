def merge(a: list, b: list) -> list:
    c = []
    i = 0
    j = 0
    while i < len(a) or j < len(b):
        if i == len(a):
            c.append(b[j])
            j += 1
        elif j == len(b):
            c.append(a[i])
            i += 1
        elif a[i] <= b[j]:
            c.append(a[i])
            i += 1
        else:
            c.append(b[j])
            j += 1
    return c


def mergesort(x: list) -> list:
    n = len(x)
    if n < 2:
        return x
    mid = int(n / 2)
    left = mergesort(x[0:mid])
    right = mergesort(x[mid:n])
    return merge(left, right)


print(merge([1, 3], [2, 4]))
print(mergesort([1, 3, 2, 4]))
print(mergesort([1, 4, 8, 5, 6, 4, 1, 2, 5, 6, 14, 12, 11, 21, 0, 6, 7, 10]))
