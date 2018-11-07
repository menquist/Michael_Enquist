'''
Princess Peach is trapped in one of the four corners of a square grid. You are in the center of the grid and can move one step at a time in any of the four directions. Can you rescue the princess?

Input format

The first line contains an odd integer N (3 <= N < 100) denoting the size of the grid. This is followed by an NxN grid. Each cell is denoted by '-' (ascii value: 45). The bot position is denoted by 'm' and the princess position is denoted by 'p'.

Grid is indexed using Matrix Convention

Output format

Print out the moves you will take to rescue the princess in one go. The moves must be separated by '\n', a newline. The valid moves are LEFT or RIGHT or UP or DOWN.

Sample input

3
---
-m-
p--
Sample output

DOWN
LEFT
Task

Complete the function displayPathtoPrincess which takes in two parameters - the integer N and the character array grid. The grid will be formatted exactly as you see it in the input, so for the sample input the princess is at grid[2][0]. The function shall output moves (LEFT, RIGHT, UP or DOWN) on consecutive lines to rescue/reach the princess. The goal is to reach the princess in as few moves as possible.

The above sample input is just to help you understand the format. The princess ('p') can be in any one of the four corners.

Scoring 
Your score is calculated as follows : (NxN - number of moves made to rescue the princess)/10, where N is the size of the grid (3x3 in the sample testcase).
'''


#!/bin/python
# Head ends here

def displayPathtoPrincess(n,grid):
#print all the moves here
# Tail starts here
    g = Grid(n, grid)
    if not g.is_valid():
        raise Exception('Grid validation error: %s' % '\n'.join(g.errors))

    g.get_bot_coordinates()
    g.get_princess_coordinates()

    print g.get_path()


class Grid(object):

    def __init__(self, n, grid):
        self.n = n
        self.matrix = grid
        self.errors = []
        self.princess_marker = 'p'
        self.bot_marker = 'm'
        self.princess_coord = None
        self.bot_coord = None

    def has_valid_row_count(self):
        return self.n == len(self.matrix)

    def has_valid_column_count(self):
        for i in self.matrix:
            if not len(i) == self.n:
                self.errors.append('Grid expected column size %s but got %s' % (self.n, len(i)))
                return False
        return True
                
    def is_valid(self):
        r = True
        if not self.has_valid_row_count():
            self.errors.append('Grid expected row size %s but got %s' % (self.n, len(self.matrix)))
            r = False
        if not self.has_valid_column_count():
            r = False
        return r

    def get_coordinates_for(self, val):
        for x, row in enumerate(self.matrix):
            for y, col in enumerate(row):
                if col == val:
                    return (x, y)

    def get_princess_coordinates(self):
        if not self.princess_coord:
            self.princess_coord = self.get_coordinates_for(self.princess_marker)
        return self.princess_coord

    def get_bot_coordinates(self):
        if not self.bot_coord:
            self.bot_coord = self.get_coordinates_for(self.bot_marker)
        return self.bot_coord

    def get_path(self):
        path = []
        row_diff = self.bot_coord[0] - self.princess_coord[0]
        col_diff = self.bot_coord[1] - self.princess_coord[1]
        #print row_diff
        #print col_diff

        if row_diff > 0:
            row_direction = 'UP'
        else:
            row_direction = 'DOWN'

        if col_diff > 0:
            col_direction = 'LEFT'
        else:
            col_direction = 'RIGHT'
            
        for i in xrange(abs(row_diff)):
            path.append(row_direction)
        for i in xrange(abs(col_diff)):
            path.append(col_direction)

        return '\n'.join(path)


m = input()

grid = []
for i in xrange(0, m):
    grid.append(raw_input().strip())

displayPathtoPrincess(m,grid)