#! /usr/bin/env python

input_file = 'IntegerArray.txt'

def init_array():
    i = 0
    array = []
    for line in open(input_file):
        array.append(int(line))
        i += 1
    return array

def count_inversions(A):
    n = len(A)
    if n == 1:
        return A,0
    B,x = count_inversions(A[0:n/2])
    C,y = count_inversions(A[n/2:n])
    D,z = merge_sort_count(B,C)
    return D,x+y+z

def merge_sort_count(B,C):
    m = len(B)
    n = len(C)
    D = []
    i,j,count = 0,0,0
    for k in range(m+n):
        if i == m:
            D.append(C[j])
            j += 1
        elif j == n:
            D.append(B[i])
            i += 1
        else:
            if B[i] < C[j]:
                D.append(B[i])
                i += 1
            else:
                D.append(C[j])
                j += 1
                count += m-i
    return D,count

if __name__ == '__main__':
    A = init_array()
    A,count = count_inversions(A)
    print count
