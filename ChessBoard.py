#!/usr/bin/python
# -*- coding: utf-8 -*-

#######################################################################
# ChessBoard.py - Copyright 2011 Guillaume MICHON
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#######################################################################


import copy

WHITE, BLACK = range(2)
FILES = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
RANKS = range(1, 9)


class Direction(object):
    """
    Describes the delta between two coordinates
    """
    def __init__(self, x_delta, y_delta):
        self.x_delta = x_delta
        self.y_delta = y_delta

    def getVector(self):
        """
        Returns a new Direction where x_delta and y_delta in (-1, 0, 1)
        """
        return Direction( self.x_delta and self.x_delta/abs(self.x_delta),
                          self.y_delta and self.y_delta/abs(self.y_delta) )

    def getDelta(self):
        """
        Returns the number of square separated by the Direction
        """
        return max(abs(self.x_delta), abs(self.y_delta))

    def __add__(self, other):
        if not isinstance(other, Direction):
            raise Exception("Addition on Direction is only implemented with another Direction")
        return Direction(self.x_delta + other.x_delta, self.y_delta + other.y_delta)

    def __sub__(self, other):
        if not isinstance(other, Direction):
            raise Exception("Substraction on Direction is only implemented with another Direction")
        return Direction(self.x_delta - other.x_delta, self.y_delta - other.y_delta)

    def __eq__(self, other):
        return self.x_delta == other.x_delta and self.y_delta == other.y_delta

    def __mul__(self, other):
        """
        Works only with a number
        """
        return Direction(self.x_delta * other, self.y_delta * other)

    def __str__(self):
        return "Direction(%s,%s)" % (self.x_delta, self.y_delta)

    def __hash__(self):
        return self.x_delta * 1000000 + self.y_delta # FIXME: Not clean


UP    = Direction( 0 , 1)
DOWN  = Direction( 0 ,-1)
LEFT  = Direction(-1 , 0)
RIGHT = Direction( 1 , 0)
UPLEFT = UP + LEFT
UPRIGHT = UP + RIGHT
DOWNLEFT = DOWN + LEFT
DOWNRIGHT = DOWN + RIGHT
CASTLING_NOTATION = {LEFT: "O-O-O", RIGHT: "O-O"}



class Coordinate(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def toAlgebraic(self):
        if not (0 <= self.x < len(FILES) and 0 <= self.y < len(RANKS)):
            raise Exception("Call on toAlgebraic() on a out-of-bound Coordinate (%s,%s)" % (self.x, self.y))
        return "%s%s" % (FILES[self.x], RANKS[self.y])

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __add__(self, other):
        if not isinstance(other, Direction):
            raise Exception("Addition on Coordinate is only implemented with a Direction")
        return Coordinate(self.x + other.x_delta, self.y + other.y_delta)

    def __sub__(self, other):
        if isinstance(other, Direction):
            return Coordinate(self.x - other.x_delta, self.y - other.y_delta)
        if isinstance(other, Coordinate):
            return Direction(self.x - other.x, self.y - other.y)
        raise Exception("Substraction on Coordinate is only implemented with a Direction or another Coordinate")
        
    def __str__(self):
        return "Coordinate(%s,%s)" % (self.x, self.y)

    def __hash__(self):
        return self.x * 10000000 + self.y # FIXME: Not clean

    @staticmethod
    def fromAlgebraic(string):
        if len(string) != 2:
            raise Exception("len(string) must be 2 (given arg : %s)" % (repr(string)))
        try:
            x, y = string
            if x in FILES and int(y) in RANKS:
                return Coordinate(FILES.index(x), RANKS.index(int(y)))
            raise Exception("Given coordinates are out of bounds")
        except ValueError:
            raise Exception("string format must be <file><rank>. e.g. 'e4' (given arg : %s)" % repr(string))


class Square(object):
    """
    A square is the smallest portion of a board. It has unique coordinates,
    can be occupied by a Piece, and can be controlled by any number of Pieces
    """
    def __init__(self):
        self.flush(piece=True)

    def flush(self, piece=False, controlling=True, shadow=True):
        """
        Reinitialize the Square. Each parameters is a boolean
        indicating what to flush.
        """
        if piece:
            self.removePiece()
        if controlling:
            self.flushControllingPieces()
        if shadow:
            self.removeShadowPiece()

    def getPiece(self):
        return self.piece
    def setPiece(self, piece):
        if self.piece is not None:
            raise Exception("This Square is already occupied")
        self.piece = piece
    def removePiece(self):
        try:
            piece = self.piece
        except AttributeError:
            self.piece = None
            return
        self.piece = None
        return piece

    def flushControllingPieces(self):
        self.controlling_piece_set = set()
    def addControllingPiece(self, piece):
        self.controlling_piece_set.add(piece)
    def removeControllingPiece(self, piece):
        self.controlling_piece_set.remove(piece)
    def getControllingPieces(self):
        return self.controlling_piece_set

    def getShadowPiece(self):
        """
        A "shadow" Piece is a Piece which is not physically on the Square,
        but just moved through it and may be attacked or taken on it due
        to special rules (en passant, castling).
        """
        return self.shadow_piece
    def setShadowPiece(self, piece):
        if self.shadow_piece is not None:
            raise Exception("There is already another shadow piece on this Square")
        self.shadow_piece = piece
    def removeShadowPiece(self):
        self.shadow_piece = None
        

class Piece(object):
    """
    A Piece is any movable element on the board (e.g. Pawn, Rook, etc)
    """
    # self.algebraic and self.long_algebraic must be overriden
    algebraic = ""
    long_algebraic = ""
    # move_direction_list should be overriden on simple Pieces (Rook, Bishop, Queen)
    move_direction_list = []

    def __init__(self, coord, colour):
        if not isinstance(coord, Coordinate):
            raise Exception("coord must be a Coordinate instance")
        if colour not in (BLACK, WHITE):
            raise Exception("colour must be BLACK or WHITE")
        self.setInitialPosition(coord)
        self.colour = colour

    def getCopy(self):
        """
        Returns a new instance of Piece with exactly same properties.
        """
        piece = self.__class__(self.coord, self.colour)
        piece.history = copy.deepcopy(self.history)
        piece.history_index = self.history_index
        return piece

    def __str__(self):
        return "Piece %s%s" % (self.getFEN(), self.coord.toAlgebraic())


    @staticmethod
    def fromFEN(string, coord):
        """
        Returns a specialized instance (i.e. Rook, Bishop, etc...) of Piece from
        a FEN notation, at the given coord.
        """
        piece = PIECES.get(string, None)
        if piece is None:
            raise Exception("Unknown FEN notation for Piece : %s" % string)
        # "piece" is a tuple (PieceClass, colour)
        return piece[0](coord, piece[1])

    @staticmethod
    def getClassFromAlgebraic(self, letter):
        """
        Returns a specialized class (i.e. Rook, Bishop, etc...) of Piece
        from the given letter (e.g. : R -> Rook)
        """
        piece = PIECE.get(letter, None)
        if piece is None:
            raise Exception("Unknown notation : %s" % letter)
        return piece[0]
   
    def getAlgebraic(self):
        """
        Returns the short algebraic notation for this Piece
        """
        return self.algebraic
    def getLongAlgebraic(self):
        """
        Returns the long algebraic notation for this Piece
        """
        return self.long_algebraic
    def getFEN(self):
        """
        Returns the FEN notation for this Piece
        """
        fen = self.long_algebraic
        if self.colour == WHITE:
            return fen.upper()
        else:
            return fen.lower()
            
    def getShadowList(self):
        """
        Returns the list of "shadows" of the Piece
        A shadow is a coordinate where the Piece just moved through,
        but where it may be attacked anyway.
        Shadows are used with special rules :
          * En passant
          * Castling
        """
        return self.history[self.history_index]["shadow_list"]

    def updateShadowList(self, old_coord, new_coord):
        """
        This method is called by moveTo()
        Updates the list of shadows of the Piece.
        This method should be overriden and sould place shadows
        only for special rules (i.e. en passant and castling)
        """
        pass

    def setInitialPosition(self, coord):
        """
        Places the Piece on the given coordinate and records it as its initial place.
        """
        if not isinstance(coord, Coordinate):
            raise Exception("coord must be a Coordinate instance")
        self.coord = coord
        # No "potential_moves" initialization because getPotentialMoves() is used to populate other sets
        self.history = [{"moved": False, "coord": coord, "shadow_list": [], "capture_moves": set(), "controlled_coord": set()}]
        self.history_index = 0

    def getInitialPosition(self):
        """
        Returns the initial coordinates for the Piece
        """
        return self.history[0]["coord"]

    def moveTo(self, coord):
        """
        Moves the Piece to the given coordinate, incrementing its move history
        """
        if not isinstance(coord, Coordinate):
            raise Exception("coord must be a Coordinate instance")
        old_coord = self.history[self.history_index]["coord"]
        old_moved = self.history[self.history_index]["moved"]
        # Truncate history if we did an undo
        self.history = self.history[:self.history_index+1]
        # No "potential_moves" initialization because getPotentialMoves() is used to populate other sets
        self.history.append({
            "moved": old_moved or old_coord != coord,
            "coord": coord, 
            "shadow_list": [], 
            "capture_moves": set(), 
            "controlled_coord": set()
        })
        self.coord = coord
        self.history_index += 1
        self.updateShadowList(old_coord, coord)

    def hasMoved(self):
        """
        Returns True if the Piece has ever moved (since setInitialPosition() )
        """
        return self.history[self.history_index]["moved"]

    def undo(self):
        """
        Takes back to previous position, keeping in memory further ones.
        Any call to moveTo() will erase all further positions with the new one.
        """
        self.history_index = max(0, self.history_index - 1)
        self.coord = self.history[self.history_index]["coord"]

    def redo(self):
        """
        Returns to the immediate further position after a call to undo()
        """
        self.history_index = min(self.history_index+1, len(self.history)-1)
        self.coord = self.history[self.history_index]["coord"]

    def testCheck(self, board):
        """
        Tests if the Piece is in check. This method is called after an initial
        call to getControlledCoordinates() for all Pieces, so it may call
        board.isControlledBy(coord, colour)
        This method is used to determine if a the latest move was legal or not.
        Only the King can be checked, so it should be overriden only by King class
        """
        return False

    def getPotentialPromotionsOn(self, coord, board):
        """
        Returns the list of Piece classes the Piece can promote to on the given coords.
        """
        return []


    def getPotentialMoves(self, board):
        """
        Returns a list of all the coordinates where the Piece may be moved,
        without capture moves.
        This method is aware of piece move rules, obstacles and board bounds
        (by polling it), but may return illegal moves because it does not take
        checks into account.
        """
        return_set = self.history[self.history_index].get("potential_moves", None)
        if return_set is not None:
            return list(return_set)
        self.history[self.history_index]["potential_moves"] = set()
        for move_direction in self.move_direction_list:
            coord = self.coord
            stop_loop = False
            coord = coord + move_direction
            while board.isCoordInBoard(coord) and not stop_loop:
                self.history[self.history_index]["controlled_coord"].add(coord)
                target_piece = board.getPieceAtCoord(coord)
                if target_piece is not None:
                    if target_piece.colour == self.colour:
                        # Our own Piece blocks this square and further ones
                        stop_loop = True
                    else:
                        # An opponent Piece can be taken, but block further moves
                        self.history[self.history_index]["capture_moves"].add(coord)
                        stop_loop = True
                else:
                    # Square is empty
                    self.history[self.history_index]["potential_moves"].add(coord)
                coord = coord + move_direction
        return list(self.history[self.history_index]["potential_moves"])

    def getPotentialCaptureMoves(self, board):
        """
        Returns a list of all the coordinates where the Piece can capture
        an opponent Piece according to its move rules.
        It returns only squares where an opponent Piece is present.
        This method is aware of piece move rules, obstacles and board bounds
        (by polling it), but may return illegal moves because it does not take
        checks into account.
        """
        self.getPotentialMoves(board) # Populates capture_moves set
        return list(self.history[self.history_index]["capture_moves"])

    def getControlledCoordinates(self, board):
        """
        Return a list of Coordinates the Piece controls.
        Controlling a Coordinate means the Piece defends this square. The square
        may be empty or populated by a friendly or hostile Piece.
        This method is useful to know where the Kings can move (they cannot move
        on an opponent-controlled square since they would be in check)
        """
        self.getPotentialMoves(board) # Populates controlled_coord set
        return list(self.history[self.history_index]["controlled_coord"])



class ShortRangePiece(Piece):
    """
    A ShortRangePiece is different in its moving scheme :
    it does not move until an obstacle, but has only a finite
    number of options to move.
    To resume :
        * Piece has a move_DIRECTION_list attribute. Only obstacles limit target squares
        * ShortRangePiece has a move_list attribute. len(move_list) = target squares
    """
    # One should override self.move_list to easily implement a ShortRangePiece
    move_list = []

    def getPotentialMoves(self, board):
        """
        Overriden
        """
        return_set = self.history[self.history_index].get("potential_moves", None)
        if return_set is not None:
            return list(return_set)
        self.history[self.history_index]["potential_moves"] = set()
        for move in self.move_list:
            coord = self.coord + move
            if board.isCoordInBoard(coord):
                self.history[self.history_index]["controlled_coord"].add(coord)
                target_piece = board.getPieceAtCoord(coord)
                if target_piece is not None:
                    if target_piece.colour != self.colour:
                        # An opponent Piece can be taken
                        self.history[self.history_index]["capture_moves"].add(coord)
                else:
                    # Square is empty
                    self.history[self.history_index]["potential_moves"].add(coord)
        return list(self.history[self.history_index]["potential_moves"])


class Rook(Piece):
    algebraic = "R"
    long_algebraic = "R"
    move_direction_list = [UP, DOWN, LEFT, RIGHT]

class Bishop(Piece):
    algebraic = "B"
    long_algebraic = "B"
    move_direction_list = [UPLEFT, UPRIGHT, DOWNLEFT, DOWNRIGHT]

class Queen(Piece):
    algebraic = "Q"
    long_algebraic = "Q"
    move_direction_list = [UP, DOWN, LEFT, RIGHT, UPLEFT, UPRIGHT, DOWNLEFT, DOWNRIGHT]

class Knight(ShortRangePiece):
    algebraic = "N"
    long_algebraic = "N"
    move_list = [
        UP + UP + RIGHT,
        UP + UP + LEFT,
        UP + LEFT + LEFT,
        DOWN + LEFT + LEFT,
        DOWN + DOWN + LEFT,
        DOWN + DOWN + RIGHT,
        DOWN + RIGHT + RIGHT,
        UP + RIGHT + RIGHT
    ]

class King(ShortRangePiece):
    """
    Special rules for King :
        * Castle
        * Can be checked
    """
    algebraic = "K"
    long_algebraic = "K"
    move_list = [UP, DOWN, LEFT, RIGHT, UPLEFT, UPRIGHT, DOWNLEFT, DOWNRIGHT]

    def updateShadowList(self, old_coord, new_coord):
        """
        Overriden for castling rule : the King must not be checked
          * On its starting square
          * On the traversed square
          * On its landing square
        """
        direction = new_coord - old_coord
        if direction.getDelta() == 2:
            # The King only moves one square at once
            # If it has moved two squares at once, it just castled
            self.history[self.history_index]["shadow_list"] = [old_coord, old_coord + direction.getVector()]
        else:
            self.history[self.history_index]["shadow_list"] = []

    def testCheck(self, board):
        """
        Tests if the Piece is in check. This method is called after an initial
        call to getControlledCoordinates() for all Pieces, so it may call
        board.isControlledBy(coord, colour)
        This method is used to determine if a the latest move was legal or not.
        """
        for coord in [self.coord] + self.getShadowList():
            if board.isControlledBy(coord, (self.colour+1)%2):
                return True
        return False

    def getPotentialMoves(self, board):
        """
        Overriden because of castling
        Castling is a complex rule because conditions are multiple :
          * The squares between the King and the Rook must be empty
          * The King and the implied Rook must not have moved
          * The King must not be checked on the 3 implied squares
                -> This last rule is implemented via "shadows"
        """
        ShortRangePiece.getPotentialMoves(self, board) # Populates potential_moves set
        # Checking for castle
        if not self.hasMoved():
            # Looking for Rooks
            for direction in LEFT, RIGHT:
                coord = self.coord + direction
                stop_loop = False
                while board.isCoordInBoard(coord) and not stop_loop:
                    piece = board.getPieceAtCoord(coord)
                    if piece is not None:
                        if not (isinstance(piece, Rook) and piece.colour == self.colour):
                            # A Piece obstructs the path
                            stop_loop = True
                        else:
                            if piece.hasMoved():
                                # We found a Rook but it has moved
                                stop_loop = True
                            else:
                                self.history[self.history_index]["potential_moves"].add(self.coord + direction * 2)
                                stop_loop = True
                    coord = coord + direction
        return list(self.history[self.history_index]["potential_moves"])


class Pawn(Piece):
    """
    Special rules for Pawn :
        * It moves only one square ahead
        * On its first move it can move two squares ahead
        * It takes ahead-left or ahead-right
        * Promotion (managed by the board)
        * En passant
    """
    algebraic = ""
    long_algebraic = "P"

    def __init__(self, coord, colour):
        Piece.__init__(self, coord, colour)
        if self.colour == WHITE:
            self.direction = UP
        else:
            self.direction = DOWN


    def updateShadowList(self, old_coord, new_coord):
        """
        Overriden for en passant rule : the Pawn may be taken
        by another Pawn on the square juste ahead of old_coord
        """
        direction = new_coord - old_coord
        if direction.getDelta() == 2:
            # The Pawn justed made its first move, moving 2 squares ahead
            self.history[self.history_index]["shadow_list"] = [old_coord + direction.getVector()]
        else:
            self.history[self.history_index]["shadow_list"] = []


    def getPotentialMoves(self, board):
        """
        Overriden since Pawn rules are complex
        """
        return_set = self.history[self.history_index].get("potential_moves", None)
        if return_set is not None:
            return list(return_set)
        self.history[self.history_index]["potential_moves"] = set()
        coord = self.coord + self.direction
        if board.isCoordInBoard(coord):
            # It should always be in board because of promotion
            if board.getPieceAtCoord(coord) is None:
                # A Pawn cannot take ahead
                self.history[self.history_index]["potential_moves"].add(coord)

                # Now we test the second square in case of a first move
                if not self.hasMoved():
                    coord = coord + self.direction
                    if board.isCoordInBoard(coord) and board.getPieceAtCoord(coord) is None:
                        self.history[self.history_index]["potential_moves"].add(coord)
        return list(self.history[self.history_index]["potential_moves"])


    def getPotentialCaptureMoves(self, board):
        """
        Overriden since Pawn rules are complex
        """
        self.getControlledCoordinates(board) # Populates capture_move set
        return list(self.history[self.history_index]["capture_moves"])


    def getControlledCoordinates(self, board):
        """
        Overriden since Pawn rules are complex
        """
        test_coord = [self.coord + self.direction + LEFT, self.coord + self.direction + RIGHT]
        for coord in test_coord:
            if board.isCoordInBoard(coord):
                self.history[self.history_index]["controlled_coord"].add(coord)
                target_piece = board.getPieceAtCoord(coord)
                if target_piece is not None and target_piece.colour != self.colour:
                    self.history[self.history_index]["capture_moves"].add(coord)
                # Checking for en passant rule
                target_piece = board.getPieceAtCoord(coord - self.direction)
                if target_piece is not None and \
                   isinstance(target_piece, Pawn) and \
                   target_piece.colour != self.colour and \
                   coord in target_piece.getShadowList():
                       self.history[self.history_index]["capture_moves"].add(coord)
        return self.history[self.history_index]["controlled_coord"]


    def getPotentialPromotionsOn(self, coord, board):
        """
        Returns the list of Piece classes the Piece can promote to on the given coords.
        Overriden for Pawn
        """
        if board.isCoordInBoard(coord + self.direction):
            return []
        return [Queen, Rook, Knight, Bishop]


PIECES = {
    "P": (Pawn, WHITE),
    "R": (Rook, WHITE),
    "N": (Knight, WHITE),
    "B": (Bishop, WHITE),
    "Q": (Queen, WHITE),
    "K": (King, WHITE),
    "p": (Pawn, BLACK),
    "r": (Rook, BLACK),
    "n": (Knight, BLACK),
    "b": (Bishop, BLACK),
    "q": (Queen, BLACK),
    "k": (King, BLACK)
}


class ChessBoard(object):
    """
    A ChessBoard regroups Pieces on different Squares.
    It maintains an history of moves and can go backward of forward.
    """
    def __init__(self, x_size, y_size):
        self.x_size = x_size
        self.y_size = y_size
        self.initializeEmpty()

    def getCopy(self):
        """
        Return a new instance of ChessBoard with exactly same properties
        """
        board = ChessBoard(self.x_size, self.y_size)
        board.history = copy.deepcopy(self.history)
        board.history_index = self.history_index
        board.turn = self.turn
        for colour, piece_set in self.piece_dict.items():
            for piece in piece_set:
                board.addPiece(piece.getCopy())
        return board

    def initializeEmpty(self, turn=WHITE):
        """
        Initializes the Board with empty Squares
        """
        self.square_matrix = []
        self.turn = turn
        self.history = [{
            "captured_pieces": set(),   # After the last move
            "promoted_pieces": set(),   # After the last move - these are _removed_ from board because of promotion
            "future_pieces": set(),     # These pieces will appear on board on next move due to promotion
                                        #   (used by undo()/redo())
            "legal_moves": None,        # None if not calculated yet
            "start_coord":None,         # Of the last move
            "end_coord":None            # Of the last move
        }]
        self.history_index = 0
        self._controlled_square_is_calculated = False
        self.piece_dict = {WHITE:set(), BLACK:set()} # FIXME: Not generic, neither in the whole code
        for x in range(self.x_size):
            rank = []
            for y in range(self.y_size):
                rank.append(Square())
            self.square_matrix.append(rank)

    def initializeFEN(self, fen, turn=WHITE):
        """
        Initializes the Board from a FEN notation.
        WARNING : The Board is initialized, i.e. the position
        is considered as the initial position : for example, castle is allowed
        Anormal number of ranks is reported as an Exception
        Anormal number of files is ignored silently (completed or truncated)
        The position is trusted, i.e. it is considered as valid
        
        FEN Notation example :
          * rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR : Standard starting position
          * rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR : After 1.e4
            This method also supports dot notation, e.g :
          * rnbqkbnr/pppppppp/......../......../......../......../PPPPPPPP/RNBQKBNR
            Spaces and newlines are ignored, e.g :
          * r.bqkbnr/\n
            pp.ppppp/\n
            ..n...../\n
            ..p...../\n
            ...PP.../\n
            .....N../\n
            PPP..PPP/\n
            RNBQKB.R         : Sicilian opening
        """
        fen = fen.replace(" ", "").replace("\n", "")
        for i in range(1,10):
            fen = fen.replace("%i" % i, "." * i)
        rank_list = fen.split("/")
        if len(rank_list) != self.y_size:
            raise Exception("Wrong FEN format : wrong number of ranks")

        self.initializeEmpty(turn)
        for y in range(self.y_size):
            rank = rank_list[-y-1]
            for x in range(min(self.x_size, len(rank))):
                let = rank[x]
                if let != ".":
                    self.addPiece(Piece.fromFEN(let, Coordinate(x, y)))
                    
    def addPiece(self, piece):
        """
        Adds the given Piece on the Board.
        The Coordinate is taken from the Piece.
        """
        self._getSquare(piece.coord).setPiece(piece)
        self.piece_dict[piece.colour].add(piece)
        self._controlled_square_is_calculated = False

        
    def getFEN(self, dots=False):
        """
        Returns the FEN notation for the current Board
        """
        rank_list = []
        for y in range(self.y_size):
            empty_count = 0
            rank = ""
            for x in range(self.x_size):
                piece = self.getPieceAtCoord(Coordinate(x, y))
                if piece is None:
                    if dots:
                        rank += "."
                    else:
                        empty_count += 1
                else:
                    if empty_count:
                        rank += "%i" % empty_count # FIXME: Does not support empty_count > 9
                        empty_count = 0
                    rank += piece.getFEN()
            if empty_count:
                rank += "%i" % empty_count # FIXME: Does not support empty_count > 9
            rank_list.insert(0,rank)
        return "/".join(rank_list)
        

    def getPieceAtCoord(self, coord):
        """
        Returns the Piece at the given Coordinate, or None
        """
        x, y = coord.x, coord.y
        if not self.isCoordInBoard(coord):
            raise Exception("%s is not in Board" % coord)
        return self._getSquare(coord).getPiece()


    def isCoordInBoard(self, coord):
        """
        Returns a boolean indicating if the given Coordinate is in this Board
        """
        return 0 <= coord.x < self.x_size and 0 <= coord.y < self.y_size
        
    
    def isControlledBy(self, coord, colour):
        """
        Returns True if the Square at the given Coordinate is controlled
        by the player with the given colour.
        """
        self._calculateControlledSquares()
        for piece in self._getSquare(coord).getControllingPieces():
            if piece.colour == colour:
                return True
        return False


    def _getSquare(self, coord=None, x=None, y=None):
        """
        Internal method. Returns the Square at given coords
        """
        if coord is not None:
            x, y = coord.x, coord.y
        return self.square_matrix[x][y]

    def _testCastling(self, piece, start_coord, end_coord):
        """
        Internal method. Returns the Vector of castling if the move is castling.
        Else return None
        """
        if isinstance(piece, King) and (end_coord - start_coord).getDelta() > 1:
            return (end_coord - start_coord).getVector()
        return None


    def makeMove(self, move_string):
        """
        Do a real move on the board. move_string is an algebraic (long or short) move
        The move is done only if it is legal
        """
        for move in self._getLegalMoves():
            if move["long_notation"] == move_string or move["short_notation"] == move_string:
                self._doMove(move["start_coord"], move["end_coord"], move["promotion"])
                return
        raise Exception("Malformed, illegal or ambiguous move : %s" % repr(move_string))


    def undo(self, truncate=False):
        """
        Takes back to previous position, keeping in memory further ones
        (unless truncate is True)
        Any call to makeMove() will erase all further positions with the new one.
        """
        if self.history_index == 0:
            # We are at starting position, undo is impossible
            return

        # Remove any new Piece due to promotion
        current_promoted_pieces = self.history[self.history_index]["promoted_pieces"]
        previous_promoted_pieces = self.history[self.history_index-1]["promoted_pieces"]
        for promoted_piece in current_promoted_pieces - previous_promoted_pieces:
            square = self._getSquare(promoted_piece.coord)
            removed_piece = square.removePiece()
            self.piece_dict[removed_piece.colour].remove(removed_piece)
            square.setPiece(promoted_piece)
            self.piece_dict[promoted_piece.colour].add(promoted_piece)
            if not truncate:
                self.history[self.history_index-1]["future_pieces"].add(removed_piece)

        # Move back all Pieces present on the board
        for colour, piece_list in self.piece_dict.items():
            for piece in piece_list:
                self._getSquare(piece.coord).removePiece()
                piece.undo()
                self._getSquare(piece.coord).setPiece(piece)

        # Replace any captured Piece
        current_captured_pieces = self.history[self.history_index]["captured_pieces"]
        previous_captured_pieces = self.history[self.history_index-1]["captured_pieces"]
        for captured_piece in current_captured_pieces - previous_captured_pieces:
            self._getSquare(captured_piece.coord).setPiece(captured_piece)
            self.piece_dict[captured_piece.colour].add(captured_piece)

        # Then update internal properties
        self.turn = (self.turn-1)%2
        self.history_index -= 1
        self._controlled_square_is_calculated = False
        for x in range(self.x_size):
            for y in range(self.y_size):
                self._getSquare(x=x,y=y).flush()

        # Update shadows
        for colour, piece_list in self.piece_dict.items():
            for piece in piece_list:
                for shadow_coord in piece.getShadowList():
                    self._getSquare(shadow_coord).setShadowPiece(piece)

        if truncate:
            self.history = self.history[:self.history_index+1]


    def redo(self):
        """
        Returns to the immediate further position after a call to undo()
        """
        if self.history_index == len(self.history) - 1:
            # We are at the latest position, redo is impossible
            return

        # Remove any captured Piece
        current_captured_pieces = self.history[self.history_index]["captured_pieces"]
        future_captured_pieces = self.history[self.history_index+1]["captured_pieces"]
        for captured_piece in future_captured_pieces - current_captured_pieces:
            self._getSquare(captured_piece.coord).removePiece()
            self.piece_dict[captured_piece.colour].remove(captured_piece)

        # Move all Pieces present on the board to their next position
        for colour, piece_list in self.piece_dict.items():
            for piece in piece_list:
                self._getSquare(piece.coord).removePiece()
                piece.redo()
                self._getSquare(piece.coord).setPiece(piece)

        # Replace any new Piece due to promotion
        for future_piece in self.history[self.history_index]["future_pieces"]:
            square = self._getSquare(future_piece.coord)
            removed_piece = square.removePiece()
            self.piece_dict[removed_piece.colour].remove(removed_piece)
            square.setPiece(future_piece)
            self.piece_dict[future_piece.colour].add(future_piece)

        # Then update internal properties
        self.turn = (self.turn+1)%2
        self.history_index += 1
        self._controlled_square_is_calculated = False
        for x in range(self.x_size):
            for y in range(self.y_size):
                self._getSquare(x=x,y=y).flush()

        # Update shadows
        for colour, piece_list in self.piece_dict.items():
            for piece in piece_list:
                for shadow_coord in piece.getShadowList():
                    self._getSquare(shadow_coord).setShadowPiece(piece)


    def _testMove(self, start_coord, end_coord, promotion=None, test_checkmate=True):
        """
        Internal method.
        Tests the given move (Piece on start_coord goes on end_coord) and
        returns a boolean indicating if the move is legal or not.
        To determine if the move is legal, only checks are verified ;
        the given move is intended to be issued from a Piece.getPotentialMoves()
        and/or Piece.getPotentialCaptureMoves()
        Note that no validation is made on arguments (they are trusted)

        "promotion" may be a Piece class indicating that the moving Piece becomes
        a new instance of the given class after the move.

        If test_checkmate is True, verify checkmate of the tested move too.
        Checkmate is verified by testing all new potential moves from the
        newly attained position.

        Returns a dictionary of this form:
            { "legal": True/False, "capture": captured_piece/None, "check": True/False, "more_moves": True/False, "enpassant": True/False }
        """
        # Do not use a copy but _doMove()/undo() for performances
        history = self.history
        colour = self.turn
        board = self

        captured_piece, enpassant = board._doMove(start_coord, end_coord, promotion)
        board._calculateControlledSquares()
        # Verify if current player's King is checked
        for piece in board.piece_dict[colour]:
            if isinstance(piece, King) and piece.testCheck(board):
                board.undo(truncate=True)
                self.history = history
                return {"legal": False, "capture": captured_piece, "check": False, "more_moves": False, "enpassant": enpassant }
        # Verify if opponent's player King is checked
        check = False
        for piece in board.piece_dict[board.turn]:
            check = check or (isinstance(piece, King) and piece.testCheck(board))
        # This move is valid, verify checkmate if required
        if (not test_checkmate) or (not check):
            board.undo(truncate=True)
            self.history = history
            return {"legal": True, "capture": captured_piece, "check": check, "more_moves": False, "enpassant": enpassant}
        # Test checkmate
        more_moves = len(board._getLegalMoves(test_checkmate=False, fast=True)) > 0
        board.undo(truncate=True)
        self.history = history
        return {"legal": True, "capture": captured_piece, "check": check, "more_moves": more_moves, "enpassant": enpassant }


    def _doMove(self, start_coord, end_coord, promotion=None):
        """
        Internal method.
        Do all the stuff to do a move. No validation is made (arguments are trusted),
        this method must be called when we now what we do.

        "promotion" may be a Piece class indicating that the moving Piece becomes
        a new instance of the given class after the move.

        Returns a tuple describing the captured Piece if any :
            (piece, enpassant True/False)
        """
        piece = self.getPieceAtCoord(start_coord)
        if piece is None:
            raise Exception("Moving an inexistant Piece : serious problem in implementation")
        if piece.colour != self.turn:
            raise Exception("Moving a Piece while it is not this player's turn : serious problem in implementation")
        enpassant = False
        captured_piece = self.getPieceAtCoord(end_coord)
        if captured_piece is None:
            captured_piece = self._getSquare(end_coord).getShadowPiece()
            enpassant = captured_piece is not None
        if captured_piece is not None and piece.colour == captured_piece.colour:
            raise Exception("Doing a self-capture move : serious problem in implementation")
        # Truncate history if we did an undo
        if self.history_index != len(self.history)-1:
            self.history = self.history[:self.history_index+1]
            self.history[self.history_index]["future_pieces"] = set()
        # Then initialize next values of history
        self.history.append(
            {"captured_pieces": self.history[self.history_index]["captured_pieces"].copy(),
             "promoted_pieces": self.history[self.history_index]["promoted_pieces"].copy(),
             "future_pieces": set(),
             "legal_moves": None,
             "start_coord": start_coord,
             "end_coord": end_coord }
        )
        self.history_index += 1
        self._controlled_square_is_calculated = False
        self.turn = (self.turn+1)%2
        for x in range(self.x_size):
            for y in range(self.y_size):
                self._getSquare(x=x,y=y).flush()

        # Remove the taken Piece
        if captured_piece is not None:
            self._getSquare(captured_piece.coord).removePiece()
            self.piece_dict[captured_piece.colour].remove(captured_piece)
            self.history[self.history_index]["captured_pieces"].add(captured_piece)
        # Real move
        piece.moveTo(end_coord)
        self._getSquare(start_coord).removePiece()
        if promotion is None:
            self._getSquare(end_coord).setPiece(piece)
        else:
            # Promote the Piece
            self.history[self.history_index]["promoted_pieces"].add(piece)
            self.piece_dict[piece.colour].remove(piece)
            piece = promotion(end_coord, piece.colour)
            self.piece_dict[piece.colour].add(piece)
            self._getSquare(end_coord).setPiece(piece)

        # Castling ?
        # FIXME: A little weird
        second_piece = None
        direction = self._testCastling(piece, start_coord, end_coord)
        if direction is not None:
            rook_square = None
            # FIXME: This loop supposes Rooks are in corners. True in normal games, but not generic
            for corner_coord in (Coordinate(x=0, y=start_coord.y), Coordinate(x=self.x_size-1, y=start_coord.y)):
                if (corner_coord - start_coord).getVector() == direction:
                    rook_square = self._getSquare(corner_coord)
            if rook_square is None:
                raise Exception("Cannot find corner square : serious problem in implementation")
            second_piece = rook_square.getPiece()
            if not isinstance(second_piece, Rook):
                raise Exception("Castling with an inexistant Rook : serious problem in implementation")
            # The King moves 2 squares toward the Rook, and the Rook moves on the
            # other side of the King, so the landing square for the Rook is the square
            # where the King goes through
            rook_end_coord = start_coord + direction
            # Rook real move
            rook_square.removePiece()
            self._getSquare(rook_end_coord).setPiece(second_piece)
            second_piece.moveTo(rook_end_coord)

        # Moves inplace all other Pieces in order to reset potential moves
        for colour, piece_list in self.piece_dict.items():
            for other_piece in piece_list:
                if other_piece not in (piece, second_piece):
                    other_piece.moveTo(other_piece.coord)
                for shadow_coord in other_piece.getShadowList():
                    self._getSquare(shadow_coord).setShadowPiece(other_piece)
        return (captured_piece, enpassant)

    
    def getLegalMoves(self, move_format="long", test_checkmate=True):
        """
        Returns a list of strings indicating legal moves on this position.
        This method is aware of player turns and all chess rules.
        "format" can be :
          * "long" : e.g. "Pe2-e4", "Qd1xd4", "Pe7-e8=Q+", "Qe1-e8#"
          * "short" : e.g. "e4",    "Qxd4",   "e8=Q+",     "Qe8#"
                            with disambiguation if needed
          * "both" : e.g { "Pe2-e4": "e4", "Pe7-e8=Q+": "e8=Q+" }
          * if move_format is appended by " ep", add " e.p." to en passant
        """
        # Implementation :
        # 1) For each Piece, get all potential moves (normal moves and capture moves)
        # 2) For each move :
        #   a) Copy the board
        #   b) Apply the move on the copy
        #   c) Calculate new controlled squares for both camps
        #   d) Test if current player's King is on an opponent-controlled square. True -> not legal, end for
        #   e) Call getLegalMoves(test_checkmate=False) from this new position. If no legal move :
        #     -> If current player's opponent's King is on a current-player-controlled-square -> checkmate
        #     -> Else -> stalemate
        enpassant = False
        if move_format.endswith(" ep"):
            enpassant = True
            move_format = move_format[:-3]

        if move_format == "long":
            if enpassant:
                return_list = []
                for move in self._getLegalMoves(test_checkmate):
                    return_list.append(move["long_notation"])
                    if move["enpassant"]:
                        return_list[-1] += " e.p."
                return return_list
            return [m["long_notation"] for m in self._getLegalMoves(test_checkmate)]
        if move_format == "short":
            if enpassant:
                return_list = []
                for move in self._getLegalMoves(test_checkmate):
                    return_list.append(move["short_notation"])
                    if move["enpassant"]:
                        return_list[-1] += " e.p."
                return return_list
            return [m["short_notation"] for m in self._getLegalMoves(test_checkmate)]
        if move_format == "both":
            l = "long"
            s = "short"
            if enpassant:
                l += " ep"
                s += " ep"
            return dict( zip(
                self.getLegalMoves(l, test_checkmate),
                self.getLegalMoves(s, test_checkmate)
            ))
        raise Exception("Unknown notation format : %s" % repr(move_format))


    def _generateLegalMoveNotation(self):
        """
        Internal method. It is called only be _getLegalMoves()
        Its goal is to generate long and short notation for each possible move
        and add it in the internal object representing legal moves.
        """
        legal_moves = self.history[self.history_index]["legal_moves"]
        # First pass to generate long notation, and to detect possible ambiguous moves
        move_dict = {}
        for move in legal_moves:
            piece, start_coord, end_coord = move["piece"], move["start_coord"], move["end_coord"]
            # Test castling
            castling = self._testCastling(piece, start_coord, end_coord)
            if castling is not None:
                # Castling has a special notation, e.g. O-O
                move_string = CASTLING_NOTATION[castling]
            else:
                move_string = piece.getLongAlgebraic()
                move_string += start_coord.toAlgebraic()
                move_string += move["capture"] and "x" or "-"
                move_string += end_coord.toAlgebraic()
            if move["promotion"] is not None:
                move_string += "=%s" % move["promotion"].algebraic
            if move["check"]:
                if move["more_moves"]:
                    move_string += "+"
                else:
                    move_string += "#"
            if castling is None or not isinstance(piece, Pawn):
                # Pawns have a special short notation, e.g. exd5, which cannot be ambiguous
                # Castling has a special notation, e.g. O-O, which cannot be ambiguous
                piece = piece.getAlgebraic()
                my_file, my_rank = start_coord.toAlgebraic()
                amb_dict = move_dict.get((piece, end_coord), {"number": 0, "file":{}, "rank":{}})
                amb_dict["file"][my_file] = amb_dict["file"].get(my_file, 0) + 1
                amb_dict["rank"][my_rank] = amb_dict["rank"].get(my_rank, 0) + 1
                amb_dict["number"] += 1
                move_dict[(piece, end_coord)] = amb_dict
            move["long_notation"] = move_string

        # Second pass to generate unambiguous short notation
        for move in legal_moves:
            piece, start_coord, end_coord = move["piece"], move["start_coord"], move["end_coord"]
            # Test castling
            castling = self._testCastling(piece, start_coord, end_coord)
            if castling is not None:
                # Castling has a special notation, e.g. O-O, which cannot be ambiguous
                move_string = CASTLING_NOTATION[castling]
            elif isinstance(piece, Pawn):
                # Pawns have a special notation, e.g. exd5, which cannot be ambiguous
                move_string = end_coord.toAlgebraic()
                if move["capture"]:
                    move_string = "%sx" % start_coord.toAlgebraic()[0] + move_string
            else:
                move_string = piece.getAlgebraic()
                if move_dict[(move_string, end_coord)]["number"] > 1:
                    # We first disambiguate on file, then on rank if needed
                    my_file, my_rank = start_coord.toAlgebraic()
                    ambiguous_file = move_dict[(move_string, end_coord)]["file"][my_file] > 1
                    ambiguous_rank = move_dict[(move_string, end_coord)]["rank"][my_rank] > 1
                    if ambiguous_file:
                        # Specifying only the file is not enough
                        if ambiguous_rank:
                            # Specifying only the rank is not enough
                            move_string += my_file + my_rank
                        else:
                            move_string += my_rank
                    else:
                        move_string += my_file
                # Disambiguation is done
                move_string += move["capture"] and "x" or ""
                move_string += end_coord.toAlgebraic()
            if move["promotion"] is not None:
                move_string += "=%s" % move["promotion"].algebraic
            if move["check"]:
                if move["more_moves"]:
                    move_string += "+"
                else:
                    move_string += "#"
            move["short_notation"] = move_string


    def _getLegalMoves(self, test_checkmate=True, fast=False):
        """
        Internal method
        Returns a list of dictionaries indicating legal moves on this position.
        If fast is True, returns as soon as a legal move is discovered, and the
        return value is [None] (used to verify a checkmate)
        """
        legal_moves = self.history[self.history_index]["legal_moves"]
        if legal_moves is not None:
            return legal_moves
        legal_moves = []
        for piece in list(self.piece_dict[self.turn]):
            for move in piece.getPotentialMoves(self) + piece.getPotentialCaptureMoves(self):
                promotion_list = piece.getPotentialPromotionsOn(move, self) or [None]
                for promotion in promotion_list:
                    result = self._testMove(piece.coord, move, promotion, test_checkmate=test_checkmate)
                    if result["legal"] and not isinstance(result["capture"], King): # Capturing King is not allowed
                        if fast:
                            # In fast mode, we only return a non-empty list to indicate there are possible moves
                            return [None]
                        legal_moves.append({
                            "piece": piece,
                            "start_coord": piece.coord,
                            "end_coord": move,
                            "capture": result["capture"],
                            "promotion": promotion,
                            "check": result["check"],
                            "more_moves": result["more_moves"],
                            "long_notation": None,
                            "short_notation" : None,
                            "enpassant": result["enpassant"]
                        })
        self.history[self.history_index]["legal_moves"] = legal_moves
        self._generateLegalMoveNotation()
        return legal_moves
        
        
    def _calculateControlledSquares(self):
        """
        Internal method used to calculate and store only once the controlled squares.
        It scans the Board and get the controlled Coordinates for each found Piece
        """
        if self._controlled_square_is_calculated:
            return
        for colour, piece_set in self.piece_dict.items():
            for piece in piece_set:
                # Sanity check
                if self._getSquare(piece.coord).getPiece() != piece:
                    raise Exception("More than one Piece on a given Square, there is a big problem in code !")
                for coord in piece.getControlledCoordinates(self):
                    self._getSquare(coord).addControllingPiece(piece)
        self._controlled_square_is_calculated = True

# TODO : ChessBoard.seekFEN()
# TODO : ChessBoard.seekPossibleMove()



if __name__ == "__main__":
    print "-- Test 1 : Build a 8x8 ChessBoard without Pieces"
    board = ChessBoard(8,8)
    fen = board.getFEN()
    #print "FEN : %s" % repr(fen)
    assert fen == "8/8/8/8/8/8/8/8"
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert moves == []


    print "\n\n-- Test 2 : Build a standard starting position from a standard FEN"
    board.initializeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
    fen = board.getFEN()
    #print "FEN : %s" % repr(fen)
    assert fen == "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    expected_moves = ["Pa2-a3", "Pa2-a4", "Pb2-b3", "Pb2-b4", "Pc2-c3", "Pc2-c4",
        "Pd2-d3", "Pd2-d4", "Pe2-e3", "Pe2-e4", "Pf2-f3", "Pf2-f4", "Pg2-g3", "Pg2-g4",
        "Ph2-h3", "Ph2-h4", "Nb1-a3", "Nb1-c3", "Ng1-f3", "Ng1-h3"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["a3", "a4", "b3", "b4", "c3", "c4", "d3", "d4", "e3", "e4",
        "f3", "f4", "g3", "g4", "h3", "h4", "Na3", "Nc3", "Nf3", "Nh3"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)


    print "\n\n-- Test 3 : Build a standard starting position from a dot-notation FEN"
    board.initializeFEN("rnbqkbnr/pppppppp/......../......../......../......../PPPPPPPP/RNBQKBNR")
    fen = board.getFEN()
    assert fen == "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"


    print "\n\n-- Test 4 : Build a standard starting position from a mixed FEN with CR"
    board.initializeFEN("""
        rnbqkbnr/
        pppppppp/
        .....3/.5../
        ......../
        ......../PPPPPPPP/RNBQKBNR

        """)
    assert board.getFEN() == "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"


    print "\n\n-- Test 5 : Build a board after 1.e4"
    board.initializeFEN("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR", turn=BLACK)
    fen = board.getFEN()
    #print "FEN : %s" % repr(fen)
    assert fen == "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR"
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    expected_moves = ["Pa7-a6", "Pa7-a5", "Pb7-b6", "Pb7-b5", "Pc7-c6", "Pc7-c5",
        "Pd7-d6", "Pd7-d5", "Pe7-e6", "Pe7-e5", "Pf7-f6", "Pf7-f5", "Pg7-g6", "Pg7-g5",
        "Ph7-h6", "Ph7-h5", "Nb8-a6", "Nb8-c6", "Ng8-f6", "Ng8-h6"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["a6", "a5", "b6", "b5", "c6", "c5", "d6", "d5", "e6", "e5",
        "f6", "f5", "g6", "g5", "h6", "h5", "Na6", "Nc6", "Nf6", "Nh6"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)


    print "\n\n-- Test 6 : Moves 1.e4 d5 from a starting position"
    board.initializeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
    board.makeMove("Pe2-e4")
    fen = board.getFEN()
    #print "FEN after 1.e4 : %s" % repr(fen)
    assert fen == "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR"
    board.makeMove("d5")
    fen = board.getFEN()
    #print "FEN after 1.e4 d5 : %s" % repr(fen)
    assert fen == "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR"
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    expected_moves = ["Pa2-a3", "Pa2-a4", "Pb2-b3", "Pb2-b4", "Pc2-c3", "Pc2-c4",
        "Pd2-d3", "Pd2-d4", "Pe4-e5", "Pe4xd5", "Pf2-f3", "Pf2-f4", "Pg2-g3", "Pg2-g4",
        "Ph2-h3", "Ph2-h4", "Nb1-a3", "Nb1-c3", "Ng1-f3", "Ng1-h3", "Ng1-e2", "Ke1-e2",
        "Bf1-e2", "Bf1-d3", "Bf1-c4", "Bf1-b5+", "Bf1-a6",
        "Qd1-e2", "Qd1-f3", "Qd1-g4", "Qd1-h5"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["a3", "a4", "b3", "b4", "c3", "c4", "d3", "d4", "e5", "exd5",
        "f3", "f4", "g3", "g4", "h3", "h4", "Na3", "Nc3", "Nf3", "Nh3", "Ne2", "Ke2", 
        "Be2", "Bd3", "Bc4", "Bb5+", "Ba6", "Qe2", "Qf3", "Qg4", "Qh5"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)


    print "\n\n-- Test 7 : Test legal moves after 1.e4 d5 2.Bb5+ - from a copied board"
    board7 = board.getCopy()
    board7.makeMove("Bb5+")
    fen = board7.getFEN()
    #print "FEN after 1.e4 d5 2.Bf5+ : %s" % repr(fen)
    assert fen == "rnbqkbnr/ppp1pppp/8/1B1p4/4P3/8/PPPP1PPP/RNBQK1NR"
    moves = board7.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    expected_moves = ["Pc7-c6", "Nb8-c6", "Nb8-d7", "Bc8-d7", "Qd8-d7"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    moves = board7.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["c6", "Nc6", "Nd7", "Bd7", "Qd7"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)


    print "\n\n-- Test 8 : Test en-passant rule - from the original board"
    board.makeMove("e5")
    board.makeMove("f5")
    fen = board.getFEN()
    #print "FEN after 1.e4 d5 2.e5 f5 : %s" % repr(fen)
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    expected_moves = ["Pa2-a3", "Pa2-a4", "Pb2-b3", "Pb2-b4", "Pc2-c3", "Pc2-c4",
        "Pd2-d3", "Pd2-d4", "Pe5-e6", "Pe5xf6", "Pf2-f3", "Pf2-f4", "Pg2-g3", "Pg2-g4",
        "Ph2-h3", "Ph2-h4", "Nb1-a3", "Nb1-c3", "Ng1-f3", "Ng1-h3", "Ng1-e2", "Ke1-e2",
        "Bf1-e2", "Bf1-d3", "Bf1-c4", "Bf1-b5+", "Bf1-a6",
        "Qd1-e2", "Qd1-f3", "Qd1-g4", "Qd1-h5+"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["a3", "a4", "b3", "b4", "c3", "c4", "d3", "d4", "e6", "exf6",
        "f3", "f4", "g3", "g4", "h3", "h4", "Na3", "Nc3", "Nf3", "Nh3", "Ne2", "Ke2",
        "Be2", "Bd3", "Bc4", "Bb5+", "Ba6", "Qe2", "Qf3", "Qg4", "Qh5+"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    moves = board.getLegalMoves("short ep")
    #print "Legal moves (short ep) : %s" % repr(moves)
    expected_moves = ["a3", "a4", "b3", "b4", "c3", "c4", "d3", "d4", "e6", "exf6 e.p.",
        "f3", "f4", "g3", "g4", "h3", "h4", "Na3", "Nc3", "Nf3", "Nh3", "Ne2", "Ke2",
        "Be2", "Bd3", "Bc4", "Bb5+", "Ba6", "Qe2", "Qf3", "Qg4", "Qh5+"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    # Takes en passant
    board.makeMove("exf6")
    fen = board.getFEN()
    #print "FEN after 3. exf6 e.p. : %s" % repr(fen)
    assert fen == "rnbqkbnr/ppp1p1pp/5P2/3p4/8/8/PPPP1PPP/RNBQKBNR"
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    expected_moves = ["Pa7-a6", "Pa7-a5", "Pb7-b6", "Pb7-b5", "Pc7-c6", "Pc7-c5",
        "Pd5-d4", "Pe7-e6", "Pe7xf6", "Pe7-e5", "Pg7-g6", "Pg7xf6", "Pg7-g5", "Ph7-h6",
        "Ph7-h5", "Nb8-a6", "Nb8-c6", "Nb8-d7", "Ng8xf6", "Ng8-h6", "Bc8-d7", "Bc8-e6",
        "Bc8-f5", "Bc8-g4", "Bc8-h3", "Ke8-d7", "Ke8-f7", "Qd8-d7", "Qd8-d6"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["a6", "a5", "b6", "b5", "c6", "c5", "d4", "e6", "exf6", "e5",
        "g6", "gxf6", "g5", "h6", "h5", "Na6", "Nc6", "Nd7", "Nxf6", "Nh6", "Bd7", "Be6",
        "Bf5", "Bg4", "Bh3", "Kd7", "Kf7", "Qd7", "Qd6"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    # En passant only allowed on the immediate next move
    board.initializeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
    board.makeMove("Pe2-e4")
    board.makeMove("d5")
    board.makeMove("e5")
    board.makeMove("f5")
    board.makeMove("a3")
    board.makeMove("a6")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    expected_moves = ["Pa3-a4", "Pb2-b3", "Pb2-b4", "Pc2-c3", "Pc2-c4",
        "Pd2-d3", "Pd2-d4", "Pe5-e6", "Pf2-f3", "Pf2-f4", "Pg2-g3", "Pg2-g4",
        "Ph2-h3", "Ph2-h4", "Nb1-c3", "Ng1-f3", "Ng1-h3", "Ng1-e2", "Ke1-e2",
        "Bf1-e2", "Bf1-d3", "Bf1-c4", "Bf1-b5+", "Bf1xa6", "Ra1-a2",
        "Qd1-e2", "Qd1-f3", "Qd1-g4", "Qd1-h5+"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["a4", "b3", "b4", "c3", "c4", "d3", "d4", "e6", 
        "f3", "f4", "g3", "g4", "h3", "h4", "Nc3", "Nf3", "Nh3", "Ne2", "Ke2", "Ra2",
        "Be2", "Bd3", "Bc4", "Bb5+", "Bxa6", "Qe2", "Qf3", "Qg4", "Qh5+"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)



    print "\n\n-- Test 9 : Castling - basic move"
    board.initializeFEN("3k4/8/8/8/8/8/8/R3K2R")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert set(["O-O", "O-O-O+"]) < set(moves)
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    assert set(["O-O", "O-O-O+"]) < set(moves)
    board.makeMove("O-O")
    fen = board.getFEN()
    #print "FEN after O-O : %s" % repr(fen)
    assert fen == "3k4/8/8/8/8/8/8/R4RK1"


    print "\n\n-- Test 10 : Castling - forbidden after a move"
    # Rook move
    board.undo()
    board.makeMove("Ra8+")
    board.makeMove("Ke7")
    moves = board.getLegalMoves()
    #print "Legal moves after Rook a1 has moved : %s" % repr(moves)
    assert "O-O" in moves
    assert "O-O-O" not in moves
    moves = board.getLegalMoves("short")
    #print "Legal moves after Rook a1 has moved (short) : %s" % repr(moves)
    assert "O-O" in moves
    assert "O-O-O" not in moves
    board.makeMove("Ra1")
    board.makeMove("Kf6")
    moves = board.getLegalMoves()
    #print "Legal moves after Rook a1 is replaced : %s" % repr(moves)
    assert "O-O+" in moves
    assert "O-O-O" not in moves
    moves = board.getLegalMoves("short")
    #print "Legal moves after Rook a1 is replaced (short) : %s" % repr(moves)
    assert "O-O+" in moves
    assert "O-O-O" not in moves
    # King move
    board.makeMove("Kd1")
    board.makeMove("Ke5")
    board.makeMove("Ke1")
    board.makeMove("Ke6")
    moves = board.getLegalMoves()
    #print "Legal moves after King e1 has moved : %s" % repr(moves)
    assert "O-O" not in moves
    assert "O-O-O" not in moves
    moves = board.getLegalMoves("short")
    #print "Legal moves after King e1 has moved (short) : %s" % repr(moves)
    assert "O-O" not in moves
    assert "O-O-O" not in moves


    print "\n\n-- Test 11 : Castling - forbidden if check"
    # King is checked
    board.initializeFEN("4k3/4r3/8/8/8/8/8/R3K2R")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert "O-O" not in moves
    assert "O-O-O" not in moves
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    assert "O-O" not in moves
    assert "O-O-O" not in moves
    # Traversed square is checked
    board.initializeFEN("3k4/3r4/8/8/8/8/8/R3K2R")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert "O-O" in moves
    assert "O-O-O" not in moves
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    assert "O-O" in moves
    assert "O-O-O" not in moves
    # Landing square is checked
    board.initializeFEN("6k1/6r1/8/8/8/8/8/R3K2R")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert "O-O" not in moves
    assert "O-O-O" in moves
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    assert "O-O" not in moves
    assert "O-O-O" in moves
    # Untraversed square (b1) is controlled -> castling allowed
    board.initializeFEN("1k6/1r6/8/8/8/8/8/R3K2R")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert "O-O" in moves
    assert "O-O-O" in moves
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    assert "O-O" in moves
    assert "O-O-O" in moves


    print "\n\n-- Test 12 : Castling - forbidden if obstructed"
    # An opponent Bishop on b1
    board.initializeFEN("4k3/8/8/8/8/8/8/Rb2K2R")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert "O-O" in moves
    assert "O-O-O" not in moves
    # An opponent Bishop on c1
    board.initializeFEN("4k3/8/8/8/8/8/8/R1b1K2R")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert "O-O" in moves
    assert "O-O-O" not in moves
    # An opponent Bishop on d1
    board.initializeFEN("4k3/8/8/8/8/8/8/R2bK2R")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert "O-O" in moves
    assert "O-O-O" not in moves
    # An friendly Bishop on f1
    board.initializeFEN("4k3/8/8/8/8/8/8/R3KB1R")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert "O-O" not in moves
    assert "O-O-O" in moves


    print "\n\n-- Test 13 : Checkmate and stalemate"
    # Single checkmate
    board.initializeFEN("k7/7R/8/8/8/8/K7/6R1")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert "Rg1-g8#" in moves
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    assert "Rg8#" in moves
    board.makeMove("Rg8#")
    moves = board.getLegalMoves()
    assert moves == []
    # Checkmate with a single Rook
    board.initializeFEN("1k6/ppp5/7R/8/8/8/8/K7")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert "Rh6-h8#" in moves
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    assert "Rh8#" in moves
    board.makeMove("Rh8#")
    moves = board.getLegalMoves()
    assert moves == []
    # Checkmate with a protected Piece
    board.initializeFEN("7k/8/5N2/K7/8/8/8/6R1")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert "Rg1-g8#" in moves
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    assert "Rg8#" in moves
    board.makeMove("Rg8#")
    moves = board.getLegalMoves()
    assert moves == []
    # Checkmate with King and Queen
    board.initializeFEN("3k4/7Q/3K4/8/8/8/8/8")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert set(["Qh7-h8#", "Qh7-g8#", "Qh7-d7#"]) < set(moves)
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    assert set(["Qh8#", "Qg8#", "Qd7#"]) < set(moves)
    board.makeMove("Qh8#")
    moves = board.getLegalMoves()
    assert moves == []
    # Checkmate with a pinned Piece
    board.initializeFEN("2k1n2R/8/1B6/8/5Q2/8/8/K7")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert "Qf4-c7#" in moves
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    assert "Qc7#" in moves
    board.makeMove("Qc7#")
    moves = board.getLegalMoves()
    assert moves == []
    # Checkmate on a capture
    board.initializeFEN("2k1n2R/2r5/1B6/8/5Q2/8/8/K7")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert "Qf4xc7#" in moves
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    assert "Qxc7#" in moves
    board.makeMove("Qxc7#")
    moves = board.getLegalMoves()
    assert moves == []
    # Stalemate
    board.initializeFEN("2k5/2P5/1K6/8/8/8/8/8")
    board.makeMove("Kc6")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    assert moves == []


    print "\n\n-- Test 14 : Complex forced checkmate (smoothered)"
    board.initializeFEN("k6r/ppp5/4N3/8/5Q2/8/8/4K3")
    #print "1.Nxc7+"
    board.makeMove("Nxc7+")
    moves = board.getLegalMoves("short")
    assert moves == ["Kb8"]
    #print "1. ... Kb8 2.Na6+"
    board.makeMove("Kb8")
    board.makeMove("Na6+")
    moves = board.getLegalMoves("short")
    expected_moves = ["Ka8", "Kc8"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    #print "2. ... Ka8 3.Qb8+"
    board.makeMove("Ka8")
    board.makeMove("Qb8+")
    moves = board.getLegalMoves("short")
    assert moves == ["Rxb8"]
    #print "3. ... Rxb8 4.Nc7#"
    board.makeMove("Rxb8")
    board.makeMove("Nc7#")
    moves = board.getLegalMoves("short")
    assert moves == []


    print "\n\n-- Test 15 : Promotion"
    # Single promotion
    board.initializeFEN("k7/7P/8/8/8/8/8/K7")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    expected_moves = ["Ka1-a2", "Ka1-b1", "Ka1-b2",
        "Ph7-h8=N", "Ph7-h8=B", "Ph7-h8=R+", "Ph7-h8=Q+"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["Ka2", "Kb1", "Kb2", "h8=N", "h8=B", "h8=R+", "h8=Q+"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    board.makeMove("h8=N")
    fen = board.getFEN()
    #print "FEN after h8=N %s" % repr(fen)
    assert fen == "k6N/8/8/8/8/8/8/K7"
    board.makeMove("Ka7")
    moves = board.getLegalMoves("short")
    #print "Legal moves after Ka7 (short) : %s" % repr(moves)
    expected_moves = ["Ka2", "Kb1", "Kb2", "Ng6", "Nf7"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    # Promotion on capture
    board.initializeFEN("k5n1/7P/8/8/8/8/8/K7")
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["Ka2", "Kb1", "Kb2", "h8=N", "h8=B", "h8=R", 
        "h8=Q", "hxg8=N", "hxg8=B", "hxg8=R+", "hxg8=Q+"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    # Promotion with checkmate
    board.initializeFEN("k7/3R3P/8/8/8/8/8/K7")
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    assert set(["h8=Q#", "h8=R#"]) < set(moves)
    

    print "\n\n-- Test 16 : Disambiguation on file, on rank"
    # Disambiguation on file
    board.initializeFEN("k7/pppppppp/8/K7/8/8/pppppppp/R6R")
    moves = board.getLegalMoves()
    #print "Legal moves : %s" % repr(moves)
    expected_moves = ["Ra1-b1", "Ra1-c1", "Ra1-d1", "Ra1-e1", "Ra1-f1", "Ra1-g1",
        "Rh1-b1", "Rh1-c1", "Rh1-d1", "Rh1-e1", "Rh1-f1", "Rh1-g1", "Ra1xa2", "Rh1xh2",
        "Ka5-a4", "Ka5-b4", "Ka5-b5"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["Rab1", "Rac1", "Rad1", "Rae1", "Raf1", "Rag1",
        "Rhb1", "Rhc1", "Rhd1", "Rhe1", "Rhf1", "Rhg1", "Rxa2", "Rxh2",
        "Ka4", "Kb4", "Kb5"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    # No need of disambiguation
    board.initializeFEN("k7/pppppppp/8/K7/8/8/pppppppp/3RR3")
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    assert "Rf1" in moves
    # Disambiguation on rank
    board.initializeFEN("7R/k7/8/8/8/8/K7/7R")
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["Rg8", "Rf8", "Re8", "Rd8", "Rc8", "Rb8", "Ra8+", "Rg1", "Rf1",
        "Re1", "Rd1", "Rc1", "Rb1", "Ra1", "R8h7+", "R8h6", "R8h5", "R8h4", "R8h3",
        "R8h2", "R1h7+", "R1h6", "R1h5", "R1h4", "R1h3", "R1h2", "Ka1", "Kb1",
        "Kb2", "Kb3", "Ka3"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    # Mixed need of disambiguation
    board.initializeFEN("8/k7/7R/8/8/7R/K7/8")
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["Rg6", "Rf6", "Re6", "Rd6", "Rc6", "Rb6", "Ra6+", "Rg3", "Rf3",
        "Re3", "Rd3", "Rc3", "Rb3", "Ra3+", "Rh8", "Rh7+", "R6h5", "R6h4", "Rh1", "Rh2",
        "R3h5", "R3h4", "Ka1", "Kb1", "Kb2", "Kb3", "Ka3"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
        

    print "\n\n-- Test 17 : Complex disambiguation"
    # Priority to file
    board.initializeFEN("5k2/8/8/2N5/8/6N1/8/4K3")
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["Kd1", "Kd2", "Ke2", "Kf2", "Kf1", "Na4", "Na6", "Nb7",
        "Nd7+", "Ne6+", "Nce4", "Nd3", "Nb3", "Nge4", "Nf5", "Nh5", "Nf1", "Nh1", "Ne2"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    # But disambiguation on rank if needed
    board.initializeFEN("5k2/8/8/8/8/N7/8/N3K3")
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["Kd1", "Kd2", "Ke2", "Kf2", "Kf1", "Nb5", "Nc4",
        "N3c2", "Nb1", "Nb3", "N1c2"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    # No need of disambiguation for a pinned Piece
    board.initializeFEN("5k2/8/8/2N5/7b/6N1/8/4K3")
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["Kd1", "Kd2", "Ke2", "Kf2", "Kf1", "Na4", "Na6", "Nb7",
        "Nd7+", "Ne6+", "Ne4", "Nd3", "Nb3"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)
    # Disambiguation on complete coordinates, but only when absolutely needed
    board.initializeFEN("k7/B1B5/3K4/2B5/8/8/8/8")
    moves = board.getLegalMoves("short")
    #print "Legal moves (short) : %s" % repr(moves)
    expected_moves = ["Kc6", "Kd7", "Ke7", "Ke6", "Ke5", "Kd5", "Bab8", "Bcb8", "Bd8",
        "Ba5", "Bb4", "Ba3", "Bd4", "Be3", "Bf2", "Bg1", "Bab6", "Bc7b6", "B5b6"]
    assert set(moves) == set(expected_moves)
    assert len(moves) == len(expected_moves)


    print "\n\n-- Test 18 : Undo/redo"
    def getBoardInfo(board, move=""):
        coord_piece_list = []
        for x in range(8):
            for y in range(8):
                coord_piece_list.append(board.getPieceAtCoord(Coordinate(x,y)))
        piece_list = []
        for colour in WHITE, BLACK:
            piece_list.append([piece for piece in board.piece_dict[colour]])
        return{
            "legal_moves" : board.getLegalMoves("short"),
            "fen" : board.getFEN(),
            "last_move" : move,
            "coord_piece_list" : coord_piece_list,
            "piece_list" : piece_list,
        }

    # Make a sequence of moves
    board.initializeFEN("4k2r/1p5p/8/8/8/8/P7/4K3")
    board_info_list = [getBoardInfo(board)]
    for move in ("a4", "h6", "a5", "b5", "axb6", "O-O", "b7", 
            "Rb8", "Kd1", "Rc8", "bxc8=Q+", "Kg7", "Qc6"):
        #print "Doing %s ..." % move
        board.makeMove(move)
        board_info_list.append(getBoardInfo(board, move))
    # Undo it
    #print "-"
    for i in range(len(board_info_list)-2, -1, -1):
        #print "Undoing %s ..." % board_info_list[i+1]["last_move"]
        board.undo()
        board_info = getBoardInfo(board)
        coord_piece_list = board_info["coord_piece_list"]
        piece_list = board_info["piece_list"]
        assert board_info["legal_moves"] == board_info_list[i]["legal_moves"]
        assert board_info["fen"] == board_info_list[i]["fen"]
        assert set(coord_piece_list) == set(board_info_list[i]["coord_piece_list"])
        assert len(coord_piece_list) == len(board_info_list[i]["coord_piece_list"])
        assert set(piece_list[WHITE]) == set(board_info_list[i]["piece_list"][WHITE])
        assert len(piece_list[WHITE]) == len(board_info_list[i]["piece_list"][WHITE])
        assert set(piece_list[BLACK]) == set(board_info_list[i]["piece_list"][BLACK])
        assert len(piece_list[BLACK]) == len(board_info_list[i]["piece_list"][BLACK])
    #print "-"
    # Redo it
    for i in range(len(board_info_list)):
        if board_info_list[i]["last_move"]:
            #print "Redoing %s ..." % board_info_list[i]["last_move"]
            board.redo()
            board_info = getBoardInfo(board)
            coord_piece_list = board_info["coord_piece_list"]
            piece_list = board_info["piece_list"]
            assert board_info["legal_moves"] == board_info_list[i]["legal_moves"]
            assert board_info["fen"] == board_info_list[i]["fen"]
            assert set(coord_piece_list) == set(board_info_list[i]["coord_piece_list"])
            assert len(coord_piece_list) == len(board_info_list[i]["coord_piece_list"])
            assert set(piece_list[WHITE]) == set(board_info_list[i]["piece_list"][WHITE])
            assert len(piece_list[WHITE]) == len(board_info_list[i]["piece_list"][WHITE])
            assert set(piece_list[BLACK]) == set(board_info_list[i]["piece_list"][BLACK])
            assert len(piece_list[BLACK]) == len(board_info_list[i]["piece_list"][BLACK])


    print "\n\n-- Test 19 : Replay a real game (not GM, only from my 1400-Elo...)"
    # I'm a better programmer than a chess player !
    board.initializeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
    moves = "e4 c6 d4 d5 e5 Bf5 Nf3 e6 Be2 Nd7 O-O c5 c3 cxd4 cxd4 Ne7 Nbd2 Nc6 "
    moves += "Re1 Bb4 a3 Ba5 b4 Bb6 Nb3 O-O Be3 Rc8 Bd3 Bxd3 Qxd3 f6 Bf4 h6 Qg6 "
    moves += "Qe8 Qg3 g5 h4 fxe5 Bxe5 Ncxe5 Nxe5 Rf5 Ra2 Nxe5 Rxe5 Bc7 Rae2 Bxe5 "
    moves += "Rxe5 Qf7 hxg5 hxg5 Re1 b6 a4 Rc2 f3 Rc3 Nd2 Rd3 Nf1 Rxd4 Ne3 Rxb4 "
    moves += "Rc1 d4 Nxf5 Qxf5 Qb8+ Qf8 Rc8 Rb1+ Kh2 Qxc8 Qxc8+ Kf7 Qd7+ Kf6 Qxd4+ "
    moves += "Ke7 Qg7+ Kd6 Qxg5"
    for move in moves.split():
        print "%s ..." % move
        board.makeMove(move)


    print "\n\nOK ! All test passed successful."


