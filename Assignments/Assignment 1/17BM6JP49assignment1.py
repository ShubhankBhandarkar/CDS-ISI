# Name: Shubhank Bhandarkar
# Roll No.: 17BM6JP49

import math
import numpy as np
import pandas as pd
import decimal as dml
import matplotlib.pyplot as plt

#================================================================================================================================================================
# Problem 1
# Funtion for given set of iterations:
def FindSteps1D(fun):
    linArr = []
    count = 0
    for i in range(0,1000):
        linArr.append([])
        for j in [1,10,100,1000,10000,100000,1000000]:
            linArr[count].append(fun(np.random.randint(-999,999,j)))
        count +=1
    return linArr

#=======================================================================
# Linear Search 1D:
def LinSearchPeak1D(data):
    i = 0
    steps = 1
    while (i<(len(data)-1) and data[i]<data[i+1]):
        i = i+1
        steps+=1
    return steps

# Function Call
LinSearchPeak1D(np.random.randint(-999,999,1000))

# Generating csv file for the given set of iterations
linArrData = pd.DataFrame(FindSteps1D(LinSearchPeak1D))
linArrData.columns = ['Size1','Size10','Size100','Size1000','Size10000','Size100000','Size1000000']
linArrData.to_csv('LinearSearchData.csv')

#=======================================================================
# Binary Search 1D:
def BinSearchPeak1D(arr):
    steps = beg = 0
    end = len(arr)-1
    while (beg<=end) :
        mid = round((beg+end)/2)
        if beg==end:
            steps+=1
            break
        elif arr[mid]>=arr[mid-1]:
            if arr[mid]>=arr[mid+1]:
                steps+=1
                break
            else:
                beg = mid+1
                steps+=1
        elif arr[mid]<arr[mid-1]:
            end = mid-1
            steps+=1
    return steps

# Binary Function Call
BinSearchPeak1D(np.random.randint(-999,999,100))    

# Generating csv file for the given set of iterations
binArrData = pd.DataFrame(FindSteps1D(BinSearchPeak1D))
binArrData.columns = ['Size1','Size10','Size100','Size1000','Size10000','Size100000','Size1000000']
binArrData.to_csv('BinarySearchData.csv')

#================================================================================================================================================================#
# Problem 2
def SearchPeak2D(arr_2D):
    steps = col_beg = 0
    col_end = len(arr_2D[0])-1
    col_mid = round((col_beg+col_end+1)/2)
    while (col_mid >= 0):
        a = list(arr_2D[:,col_mid])
        c_max_i = a.index(max(a))
        steps += len(a)-1
        if col_mid < len(arr_2D[0])-1:
            if arr_2D[c_max_i][col_mid] >= arr_2D[c_max_i][col_mid-1] and arr_2D[c_max_i][col_mid] >= arr_2D[c_max_i][col_mid+1]:
                steps+=1
                break
            elif arr_2D[c_max_i][col_mid+1] >= arr_2D[c_max_i][col_mid-1]:
                arr_2D = arr_2D[:,col_mid+1:]
                steps+=1
            else:
                arr_2D = arr_2D[:,0:col_mid]
                steps+=1
        elif (col_mid == (len(arr_2D[0])-1)):
            if arr_2D[c_max_i][col_mid] >= arr_2D[c_max_i][col_mid-1]:
                steps+=1
                break
            else:
                arr_2D = arr_2D[:,0:col_mid]
                steps+=1
        elif col_mid == 0:
            steps+=1
        col_end = len(arr_2D[0])-1
        col_mid = round((col_beg+col_end+1)/2)
    return steps


# Function Call
SearchPeak2D(np.array([[np.random.randint(-999,999) for j in range(1000)] for i in range(1000)]))

# Generating csv file for given set of iterations
Arr = []
count = 0
for i in range(0,1000):
    Arr.append([])
    for j in [1,10,100,1000]:
        Arr[count].append(SearchPeak2D(np.array([[np.random.randint(-999,999) for k in range(j)] for l in range(j)])))
    count +=1

ArrData2D = pd.DataFrame(Arr)
ArrData2D.columns = ['Size1','Size10','Size100','Size1000']
ArrData2D.to_csv('SearchData2D.csv')

#================================================================================================================================================================
# Problem 3

# Square Root by Binary Method:
# Creating a Function:
def BinarySquareRoot(number, guess, precision):
	if guess<0: guess=0
	start = guess
	err = end = number
	root_array = errArr = np.array([])
	if guess**2 >number:
		print("Invalid Guess. Please guess a smaller value.")
	else:
		while (err > 10**(-(precision+2))):
			mid = (start+end)/2
			err = abs((mid-guess)/mid)
			guess = mid
			root_array = np.append(root_array, mid) 
			errArr = np.append(errArr, err)
			if mid**2 == number:
				mid
				break
			elif mid**2 < number:
				start = mid
			else:
				end = mid
		return (root_array,errArr)

# Part(a):
# Function Call and printing the computed square root
rootOf = 7
guess = 0
precision = 12
binArr = BinarySquareRoot(rootOf,guess,precision)
print("Root of %d with %d precision is:" %(rootOf, precision))
print(round(dml.Decimal(BinarySquareRoot(rootOf,guess,precision)[0][len(binArr[0])-1]),precision))
print("Value Computed by built-in Square Root Function is:")
round(math.sqrt(rootOf),precision)

# Part (b): Approximate Value of Square Root & Error After each iteration:
pd.DataFrame({"Roots" : binArr[0], "Errors" : binArr[1]})
plt.plot(binArr[1])

#==============================================================================
# Square Root by Newton-Raphson Approach:
# Part(a):
def NewtonSquareRoot(number, start, precision):
    err = end = number
    rootArr = errArr = np.array([])
    while (err > 10**(-(precision))):
        mid = (start+end)/2
        rootArr = np.append(rootArr, mid) 
        err = abs((mid-start)/mid)
        errArr = np.append(errArr, err)
        start = number/mid
        end = mid
    return (rootArr,errArr)

# Function Call and printing the computed square root
rootOf = 23
guess = 0
precision = 9
nrSqrRoot = NewtonSquareRoot(rootOf,guess,precision)
print("Root of %d with %d precision is:" %(rootOf, precision))
print(round(dml.Decimal(NewtonSquareRoot(rootOf,guess,precision)[0][len(nrSqrRoot[0])-1]),precision))
print("Value Computed by built-in Square Root Function is:")
round(math.sqrt(rootOf),precision)

# Part (b): Approximate Value of Square Root & Error After each iteration:
pd.DataFrame({"Roots" : nrSqrRoot[0], "Errors" : nrSqrRoot[1]})
plt.plot(nrSqrRoot[1])

#================================================================================================================================================================
# Problem 4
def NewtonFindRoot(fun,guess,tolerance):
    der_fun = fun.deriv()
    err = 1
    errArr = value = np.array([])
    while (err > 10**(-(tolerance))):
        x = guess - fun(guess)/der_fun(guess)
        err = abs((x-guess)/x)
        guess = x
        value = np.append(value,x)
        errArr = np.append(errArr,err)
    return (value, errArr)

# Function Call:
fun = np.poly1d([1,2,3,3,-13])
guess = 1
tolerance = 10
newton_array = NewtonFindRoot(fun,guess,tolerance)
print("Root of function \n %s \nwith %d precision is:" %(fun, tolerance))
print(round(dml.Decimal(NewtonFindRoot(fun,guess,tolerance)[0][len(newton_array[0])-1]),tolerance))
#print("Value Computed by built-in Root Function is:")
#round(fun.r[3],precision)

# Part (b): Approximate Value of Square Root & Error After each iteration:
pd.DataFrame({"Roots" : newton_array[0], "Errors" : newton_array[1]})
# Convergence Rate:
newton_array[0][len(newton_array[0])-1]/(newton_array[0][len(newton_array[0])-2])**2
plt.plot(newton_array[1])

#================================================================================================================================================================
# Problem 5
def SecantFindRoot(fun, x0, x1, tolerance):
    errArr = value = np.array([])
    err = 1
    while (err > 10**(-(tolerance))):
        x = x1 - fun(x1)*((x1-x0)/(fun(x1)-fun(x0)))
        err = abs((x-x1)/x)
        x0 = x1
        x1 = x
        value = np.append(value,x)
        errArr = np.append(errArr,err)
    return (value, errArr)


# Function Call:
fun = np.poly1d([1,2,3,3,-13])
guess1 = 0
guess2 = 10
tolerance = 10
secant_array = SecantFindRoot(fun=fun,x0=guess1,x1=guess2,tolerance=tolerance)
print("Root of function \n %s \nwith %d precision is:" %(fun, tolerance))
print(round(dml.Decimal(SecantFindRoot(fun=fun,x0=guess1,x1=guess2,tolerance=tolerance)[0][len(secant_array[0])-1]),tolerance))

# Part (b): Approximate Value of Square Root & Error After each iteration:
pd.DataFrame({"Roots" : secant_array[0], "Errors" : secant_array[1]})
# Convergence Rate:
secant_array[0][len(secant_array[0])-1]/(secant_array[0][len(secant_array[0])-2])**2
plt.plot(secant_array[1])

#================================================================================================================================================================
