#! /usr/bin/env python

input_file = 'QuickSort.txt'

comparisons = 0

def load_array(in_file):
	array = []
	for line in open(in_file):
		array.append(int(line))
	return array

def quick_sort(array,l,r):
	if l < r:
		p_i = choose_pivot(array,l,r)
		swap(array,l,p_i)
		partition(array,l,r)


def partition(array,l,r):
	#l is already swapped as the pivot
	p = array[l]
	i = l + 1
	for j in range(l+1,r+1):
		global comparisons
		comparisons = comparisons+1
		if array[j] < p:
			swap(array,j,i)
			i = i+1
	swap(array,l,i-1)
	quick_sort(array,l,i-2)
	quick_sort(array,i,r)

def swap(array,i,j):
	if i != j:
		temp = array[i]
		array[i] = array[j]
		array[j] = temp

def choose_pivot(array,l,r):
	#Question 1
	#return l
	
	#Question 2
	#return r

	#Question 3
	mid = l+(r-l+1)/2+(r-l+1)%2-1
	a = array[l]
	b = array[mid]
	c = array[r]

	if a > b:
		if b > c:
			return mid
		elif a > c:
			return r
		else:
			return l
	else:
		if b < c:
			return mid
		elif a > c:
			return l
		else:
			return r


if __name__ == '__main__':
    A = load_array(input_file)
    quick_sort(A,0,len(A)-1)
    print comparisons