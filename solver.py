
from operator import concat
from numpy import False_, asmatrix, concatenate
from sympy import false
import numpy as np


def duplicate(matrix):
    dup = []
    for r in range(len(matrix)):
        dup.append([])
        for c in range(len(matrix[0])):
            dup[r].append(matrix[r][c])
    return dup


def getIndex(matrix, cell):
    for row in matrix:
        if row.__contains__(cell):
            i = matrix.index(row)
            j = row.index(cell)
            return i, j
    return False


def containsMatrix(matrix, cell):
    for row in matrix:
        if row.__contains__(cell):
            return True
    return False


def getAdjList(matrix, cell):
    currentPos = getIndex(matrix, cell)
    directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    movements = []
    rows = len(matrix)
    columns = len(matrix[0])
    for direction in directions:
        move = (currentPos[0] - direction[0], currentPos[1] - direction[1])
        if move[0] >= rows or move[0] < 0 or move[1] >= columns or move[1] < 0:
            continue
        elif matrix[move[0]][move[1]] == cell+1:
            movements.clear()
            movements.append(move)
            break
        elif matrix[move[0]][move[1]] == 0:
            movements.append(move)
    return movements


def solve(matrix, max, cell):
    adj = getAdjList(matrix, cell)
    if len(adj) == 0:
        if cell == max:
            return [duplicate(matrix)]
        else:
            return []
    if matrix[adj[0][0]][adj[0][1]] == cell+1:
        return solve(matrix, max, cell+1)
    elif containsMatrix(matrix, cell+1):
        return []
    else:
        ans = []
        for x,y in adj:
            matrix[x][y] = cell + 1
            matx2 = solve(matrix, max, cell+1)
            ans.extend(matx2)
            matrix[x][y] = 0
        return ans


# if __main__ == 
# matrix = [[0, 3, 2],
#           [0, 8, 1],
#           [7, 0, 9]]
# matrix = [[0, 25, 0, 0, 3, 0, 6, 0],
#           [23, 0, 21, 0, 0, 0, 0, 0],
#           [38, 0, 29, 0, 31, 11, 1, 9],
#           [0, 39, 35, 30, 19, 32, 0, 14],
#           [49, 0, 0, 34, 0, 18, 15, 0],
#           [0, 48, 52, 41, 0, 64, 0, 16],
#           [47, 45, 0, 53, 63, 55, 56, 0],
#           [0, 44, 43, 0, 61, 60, 59, 58]]

# print(np.matrix(solve(matrix, 64, 1)[0]))
