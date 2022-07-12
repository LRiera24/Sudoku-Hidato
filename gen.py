from sys import flags
from solver import *
import numpy as np
import random

def gen (m, toDelete):
    r = len(m)
    c = len(m[0])
    
    positions = CollectAllPositions(m, r, c)
    max = len(positions)

    PlaceRandom1(m, r, c)
    # PlaceRandomMax(m, r, c, max)
    m2 = GenerateRandomSolution(m, max, 1)
    random.shuffle(positions)
    while toDelete > 0 and len(positions) >0: 
        x,y = positions[0]
        if TryRemoveOne(m2, max, x, y):
            toDelete -=1
        positions.pop(0)
    return m2

def TryRemoveOne(m2, max, x, y):
    val = m2[x][y]
    if val == 1 or val == max:
        return False
    m2[x][y] = 0
    uni = CheckUniquenessOfSolution(duplicate(m2), max, 1)
    if uni >= 2:
        m2[x][y] = val
        return False
    return True


def CollectAllPositions(m, r, c):
    ans = []
    for r1 in range(r):
        for c1 in range(c):
            if m[r1][c1] == 0:
                ans.append((r1,c1))
    return ans

def PlaceRandom1(m, r, c):
    r1 = random.randint(0, r-1)
    c1 = random.randint(0, c-1)
    while m[r1][c1] != 0:
        r1 = random.randint(0, r-1)
        c1 = random.randint(0, c-1)
    m[r1][c1] = 1

def PlaceRandomMax(m, r, c, max):
    r1 = random.randint(0, r-1)
    c1 = random.randint(0, c-1)
    while m[r1][c1] != 0:
        r1 = random.randint(0, r-1)
        c1 = random.randint(0, c-1)
    m[r1][c1] = max

def CheckUniquenessOfSolution(matrix, max, cell):
    adj = getAdjList(matrix, cell)
    if len(adj) == 0:
        if cell == max:
            return 1
        return 0
    if matrix[adj[0][0]][adj[0][1]] == cell+1:
        return CheckUniquenessOfSolution(matrix, max, cell+1)
    elif containsMatrix(matrix, cell+1):
        return 0
    else:
        random.shuffle(adj)
        amount = 0
        for x,y in adj:
            matrix[x][y] = cell + 1
            amount += CheckUniquenessOfSolution(matrix, max, cell+1)
            if amount >=2:
                return amount
            matrix[x][y] = 0
        return amount

def GenerateRandomSolution(matrix, max, cell):
    adj = getAdjList(matrix, cell)
    if len(adj) == 0:
        if cell == max:
            return duplicate(matrix)
        else:
            return []
    if matrix[adj[0][0]][adj[0][1]] == cell+1:
        return GenerateRandomSolution(matrix, max, cell+1)
    elif containsMatrix(matrix, cell+1):
        return []
    else:
        # random.shuffle(adj)
        for x,y in adj:
            matrix[x][y] = cell + 1
            matx2 = GenerateRandomSolution(matrix, max, cell+1)
            if matx2 != []:
                return matx2
            matrix[x][y] = 0
        return []

matrix = [[0, -1, -1, -1, -1, -1, 0],
          [0, 0, -1, -1, -1, 0, 0],
          [0, 0, 0, -1, 0, 0, 0],
          [0, 0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0, 0],
          [-1, 0, 0, 0, 0, 0, -1],
          [-1, -1, 0, 0, 0, -1, -1],
          [-1, -1, -1, 0, -1, -1, -1]]

matrix = [[0,0,0,0, 0, 0],
          [0,0,0,0, 0, 0],
          [0,0,0,0, 0, 0],
          [0,0,0,0, 0, 0],
          [0,0,0,0, 0, 0],
          [0,0,0,0, 0, 0],
         [0,0,0,0, 0, 0]]

def toPrint(matrix):
    dup = []
    for r in range(len(matrix)):
        dup.append([])
        for c in range(len(matrix[0])):
            dup[r].append(matrix[r][c])
            if dup[r][c] == -1:
                dup[r][c] = '.'
    return dup

print(np.matrix(gen(matrix, 100)))
# print(np.matrix(toPrint(matrix)))