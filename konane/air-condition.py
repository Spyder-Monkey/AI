from konane import *

class MinimaxPlayer(Konane, Player):
    def __init__(self, size, depthLimit):
        Konane.__init__(self, size)
        self.limit = depthLimit

    def initialize(self, side):
        self.side = side
        self.name = "Air Conditioning"

    def min(self, board, depth):
        # call max until depth
        # depth + 1 for max
        moves = self.generateMoves(board, self.side)
        bestMove = 0
        currentBestMove = float("inf")

        if depth > self.depthLimit:
            return self.eval(board)

        for move in moves:
            bestVal = (self.max(self.nextBoard(board, self.side, move), depth+1))
            if bestVal > currentBestMove:
                currentBestMove = bestVal
                bestMove = move

        return currentBestMove, bestMove

    def max(self, board, depth):
        # call min until depth
        # depth + 1 for min
        moves = self.generateMoves(board, self.side)
        bestMove = 0
        currentBestMove = -float("inf")

        if depth > self.depthLimit:
            return self.eval(board)

        for move in moves:
            bestVal = (self.min(self.nextBoard(board, self.side, move), depth+1))
            if bestVal > currentBestMove:
                currentBestMove = bestVal
                bestMove = move

        return currentBestMove, bestMove
                

    def getMove(self, board):
        moves = self.generateMoves(board, self.side)
        # use while loop and call on each move
        # starting board is starting node
        # Player is MAX
        # Konane is MIN
        # Call eval function when you reach max depth

        if not moves:
            return []

        values = []
        alpha = -float("inf")
        for move in moves:
            values.append(self.minimax(self.nextBoard(board, self.side, move), 1, alpha, float("inf")))
            if max(values) > alpha:
                alpha = max(values)
        return moves[values.index(max(values))]

    def minimax(self, board, depth, alpha, beta):
        if depth >= self.limit:
            return self.eval(board)
        isMax = depth % 2 == 0
        if isMax:
            next_boards = self.helper(board, self.side)
        else:
            next_boards = self.helper(board, self.opponent(self.side))

        if not next_boards:
            if isMax:
                return -float("inf")
            else:
                return float("inf")

        values = []
        new_alpha = alpha
        new_beta = beta
        for next_board in next_boards:
            if values:
                if isMax:
                    if max(values) >= beta:
                        break
                    if max(values) > alpha:
                        new_alpha = max(values)
                else:
                    if min(values) <= alpha:
                        break
                    if min(values) < beta:
                        new_beta = min(values)

            values.append(self.minimax(next_board, depth+1, new_alpha, new_beta))

        if isMax:
            return max(values)
        else:
            return min(values)

    def helper(self, board, side):
        moves = self.generateMoves(board, side)
        boards = []
        for move in moves:
            boards.append(self.nextBoard(board, side, move))
        return boards

    def eval(self, board):
        # Heuristic
        return len(self.generateMoves(board, self.side))

game = Konane(8)
game.playNGames(10, MinimaxPlayer(8, 2), MinimaxPlayer(8, 1), 0)
