
from operator import concat
from numpy import False_, asmatrix, concatenate
from sympy import false


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
    directions = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]
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
            return False
    if matrix[adj[0][0]][adj[0][1]] == cell+1:
        return solve(matrix, max, cell+1)
    elif containsMatrix(matrix, cell+1):
        return False
    else:
        ans = []
        for x,y in adj:
            matrix[x][y] = cell + 1
            matx2 = solve(matrix, max, cell+1)
            if matx2 != False:
                ans.extend(matx2)
            matrix[x][y] = 0
        if ans != []:
            return ans
        else:
            return False

matrix = [[0, 3, 2],
          [0, 8, 1],
          [7, 0, 9]]

print(solve(matrix, 9, 1))
