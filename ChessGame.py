#!/usr/bin/python
# -*- coding: utf-8 -*-

#######################################################################
# ChessGame.py - Copyright 2011 Guillaume MICHON
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


from ChessBoard import ChessBoard, Coordinate, WHITE, BLACK, UP, DOWN, LEFT, RIGHT

INITIAL_POSITION = "rnbqkbnr/pppppppp/......../......../......../......../PPPPPPPP/RNBQKBNR"
MAX_PENDING_MOVES = 3
MAX_BUFFERED_CHANGES = 10

# XXX Should be in another file
DEBUG = "DEBUG"
VERBOSE = "VERBOSE"
INFO = "INFO"
WARNING = "WARNING"
ERROR = "ERROR"
FATAL = "FATAL"


class NeedHelp(Exception):
    """
    An exception class indicating the ChessGame cannot handle correctly
    a change sequence : it needs help from a Human.
    """
    pass


def removeExactChange(sequence, change):
    """
    This function is used to remove the exact PhysicalBoardChange of the sequence
    It is necessary because __hash__ function of ChessChange is implemented
    to permit the comparaison between LogicalChange and PhysicalBoardChange.
    The side effect is that sequence.remove(change) removes an ARBITRARY change
    with same coords and final piece.
    """
    for i in range(len(sequence)):
        c = sequence[i]
        if c == change and c.index == change.index:
           sequence.pop(i)
           return

def findExactChange(sequence, change):
    """
    For the same reason, this function replaces "change in sequence" in some cases
    Returns the index of 'change' or None
    """
    for i in range(len(sequence)):
        c = sequence[i]
        if c == change and c.index == change.index:
            return i
    return None


class ChessChange(object):
    """
    A change is a partial move, e.g. a piece lands on a square
    """
    def __init__(self, piece, coord):
        self.piece = piece
        self.coord = coord
    def __hash__(self):
        # FIXME Not generic
        return ord(self.piece)*10000 + self.coord.x*100 + self.coord.y
    def __eq__(self, other):
        if isinstance(other, ChessChange):
            return self.piece == other.piece and self.coord == other.coord
        raise Exception("Comparison on ChessChange is only allowed with another ChessChange")
    

class PhysicalBoardChange(ChessChange):
    """
    Represents a change in the disposition of pieces on the physical board
    """
    def __init__(self, index, piece, old_piece, coord, board_after):
        """
        piece : A char e.g. "P", "p", K", "k", ...
        old_piece : Idem, the piece on this square before the change
        end_coord : instance of Coordinate
        board_after : A list of 8-len strings representing the board from row 1 to 8
        """
        ChessChange.__init__(self, piece, coord)
        self.old_piece = old_piece
        self.index = index
        self.board_after = board_after
        self.associated = None
        self.age = 0

    def __str__(self):
        return "PhysChange(%s,%s):%s->%s (index %s)" % (self.coord.x, self.coord.y, self.old_piece, self.piece, self.index)

    def cancel(self, change):
        """
        Cancels the given change with the current change and
        cancels the current change with the given change
        """
        if self.associated is not None:
            raise Exception("%s is already associated" % self)
        if change.associated is not None:
            raise Exception("%s is already associated" % change)
        self.associated = change
        change.associated = self

    def associate(self, thing):
        """
        Associate the given thing (move, string) with this change
        """
        if self.associated is not None:
            raise Exception("%s is already associated with %s" % (self, self.associated))
        self.associated = thing

    def unassociate(self):
        """
        Remove association
        """
        if self.associated is None:
            raise Exception("%s is not associated" % self)
        self.associated = None


class LogicalChange(ChessChange):
    """
    A LogicalChange is different from a PhysicalBoardChange because
    it may or may not occur, while a PhysicalBoardChange OCCURRED.
    Consequently, a LogicalChange does not store index or board_after
    """
    def __init__(self, piece, coord, awaited=False):
        ChessChange.__init__(self, piece, coord)
        self.awaited = awaited
    def __str__(self):
        return "LogicalChange(%s,%s)->%s" % (self.coord.x, self.coord.y, self.piece)


class ChessMove(object):
    """
    Represents the move of a piece on the board.
    It is intended to store the physical board changes
    until the "key" position for this move
    (i.e. the piece landing on its final square)
    """
    def __init__(self, long_notation, short_notation):
        self.long_notation = long_notation
        self.short_notation = short_notation
        self.initial_changes = set()
        self.final_changes = set()
        self.intermediate_changes = set()
        self.awaited_changes = set()
        self.associated_changes = []
        self.index = None

    def __str__(self):
        return "Move('%s'|'%s')" % (self.long_notation, self.short_notation)

    def setIndex(self, index):
        """
        Set the index of the moment when the move was created,
        in order to undo it properly
        """
        self.index = index

    def addInitialChange(self, *changes):
        """
        The initial change is the starting square becoming empty
        """
        self.initial_changes.update(changes)

    def addFinalChange(self, *changes):
        """
        The final change is the destination square getting the piece
        """
        self.final_changes.update(changes)

    def addIntermediateChange(self, *changes):
        """
        Intermediate changes are non-symmetrical changes than can occur
        during the move but which are not mandatory.
        e.g. :
            - The final square becomes empty as the taken piece leaves it
            - A Pawn arrives on the last row
        """
        self.intermediate_changes.update(changes)

    def setAwaitedChanges(self, *changes):
        """
        Awaited changes are initial changes which not occurred yet
        when the final change was detected.
        They are mandatory, so they MUST arrive soon
        """
        self.awaited_changes = set([LogicalChange(c.piece,c.coord,True) for c in changes])

    def removeAwaitedChange(self, change):
        """
        Indicates the given awaited change occurred
        """
        for aw_change in self.awaited_changes:
            if aw_change == change:
                aw_change.awaited = False
                return

    def getAwaitedChanges(self, initial=False):
        """
        Return the remaining awaited changes
        If initial is True, return all awaited changes, even
        if they have been removed
        """
        if initial:
            return list(self.awaited_changes)
        return list([c for c in self.awaited_changes if c.awaited])

    def associateChange(self, physical_change):
        """
        Associate the given PhysicalChange to this move
        """
        physical_change.associate(self)
        self.associated_changes.append(physical_change)



class CandidateMoves(object):
    """
    The goal of this class is to deduct a chess move from a sequence of
    changes like "square a2 becomes empty", "square a3 has now a white Pawn", etc
    It is done by getting all legal moves from a given position,
    then by issuing the "key" board changes during this move.
    """
    def __init__(self, board):
        self.board = board
        self.move_list = []
        for move in self.board.getLegalMoves("both ep").items():
            self._initializeMove(move)

    def _initializeMove(self, move_tuple):
        """
        Get a move from a given position and deduct key board changes
        for it. 'move_tuple' is a tuple (long_notation, short_notation)
        """
        colour = self.board.turn
        # Delete "en passant" reference
        if move_tuple[0].endswith(" e.p."):
            chess_move = ChessMove(move_tuple[0][:-5], move_tuple[1][:-5])
        else:
            chess_move = ChessMove(*move_tuple)
        
        current_dict = { 
            "intermediate_moves": set(),
        }
        move = move_tuple[0] # long notation
        if move == "O-O":
            # FIXME: Not generic
            row = colour == WHITE and 1 or 8
            move_list = [ "Ke%i-g%i" % (row, row), "Rh%i-f%i" % (row, row) ]
        elif move == "O-O-O":
            row = colour == WHITE and 1 or 8
            move_list = [ "Ke%i-c%i" % (row, row), "Ra%i-d%i" % (row, row) ]
        else:
            move_list = [move]

        for move in move_list:
            # We store only asymmetric board changes, i.e. the changes which
            # will not be reverted by a future board change during this move
            piece = move[0]
            if colour == BLACK:
                piece = piece.lower()
            start_coord = Coordinate.fromAlgebraic(move[1:3])
            chess_move.addInitialChange(LogicalChange(".",start_coord))
            end_coord = Coordinate.fromAlgebraic(move[4:6])
            if move[3] == "x":
                # Capture
                #chess_move.addIntermediateChange(LogicalChange(".",end_coord))
                if move.endswith(" e.p."):
                    # En passant is particular : the taken piece is not on landing square
                    # FIXME: Not generic
                    if colour == WHITE:
                        taking_coord = end_coord + DOWN
                    else:
                        taking_coord = end_coord + UP
                    chess_move.addInitialChange(LogicalChange(".", taking_coord))
                else:
                    # Normal capture move
                    chess_move.addInitialChange(LogicalChange(".",end_coord))
            if len(move) > 6 and move[6] == "=":
                # Promotion
                new_piece = move[7]
                chess_move.addIntermediateChange(
                    LogicalChange(".",end_coord),
                    LogicalChange(piece,end_coord)
                )
                chess_move.addFinalChange(LogicalChange(new_piece,end_coord))
            else:
                chess_move.addFinalChange(LogicalChange(piece,end_coord))
        self.move_list.append(chess_move)


    def getCandidates(self, landing_change):
        """
        Takes a landing board change, then returns a list
        of candidate moves.
        """
        return [ move for move in self.move_list if landing_change in move.final_changes ]


class ChessGame(object):
    """
    A representation of a chess game tracked by an electronic board
    A ChessGame is able to rebuild a valid game from a sequence of
    events based on physical squares
    ! Not thread-safe
    """
    # XXX A move applied by a human must not be able to be undone by automatism
    # XXX TODO Possibility to associate a change index to a move index by human (all moves before are associated to "human")
    def __init__(self, x_size, y_size, log=None, initial_position=INITIAL_POSITION, max_pending_moves=MAX_PENDING_MOVES):
        """
        max_pending_moves :
            * Maximum number of last moves awaiting for mandatory changes before calling for help
            * Maximum number of moves a non-matched change waits before calling for help
        """
        # FIXME x_size and y_size are not verified against initial_position
        self.log_function = log or self.defaultLogFunction
        self.initial_position = initial_position
        self.max_pending_moves = max_pending_moves
        self.chessboard = ChessBoard(x_size, y_size)
        self.chessboard.initializeFEN(initial_position)
        self.move_history = []
        self.change_buffer = []
        self.pending_changes = []
        self.recording = False
        self.name = None
        self.last_ambiguous_change = None
        initial_repr = initial_position.split("/")
        initial_repr.reverse()
        self.change_history = [PhysicalBoardChange(0, None, None, None, initial_repr)]
        self.change_history_index = 0

    def __str__(self):
        return "Game(%s)" % (self.name or "NoName")

    def log(self, level, message, *args):
        """
        Logs a message
        """
        self.log_function(self, level, message, *args)
    def defaultLogFunction(self, obj, level, message, *args):
        """
        Default log function : it simply prints the message on stdout
        """
        print "[%s] - %s - %s" % (obj, level, message % args)

    def getTurn(self):
        return self.chessboard.turn


    def _getPieceColour(self, piece):
        """
        Internal method return the colour of a piece
        """
        if piece in "kqrbnp":
            return BLACK
        if piece in "KQRBNP":
            return WHITE
        raise Exception("Unknown piece : %s" % piece)


    def getValidatedDotFEN(self):
        """
        Returns the current validated position in FEN notation
        with dots in place of spaces
        """
        return self.chessboard.getFEN(dots=True)

    def getPhysicalDotFEN(self):
        """
        Returns the position on board in FEN notation
        with dots in place of spaces
        """
        board_repr = self.change_history[self.change_history_index].board_after
        return_list = []
        for i in range(len(board_repr)):
            return_list.insert(0,"".join(board_repr[i]))
        return "/".join(return_list)
            

    def getPGN(self):
        """
        Returns a PGN representation of the game (without headers)
        """
        return_string = ""
        for i in range(self.history_index+1):
            turn = i/2+1
            white_turn = i%2 == 0
            if white_turn:
                return_string += " %i." % turn
            return_string += " " + self.move_list[i]["short"] # XXX use history["last_move"]
        # XXX 0-1, 1-0 or 1/2-1/2
        return return_string.lstrip()


    def record(self, order):
        """
        Turns on or off recording. order is a boolean
        """
        self.recording = order


    def update(self, update_type, *args, **kw):
        """
        Updates the current game with given data
        The parameters depend on the update_type
        """
        f = getattr(self, "_update%s" % update_type.capitalize(), None)
        if f is not None and callable(f):
            return f(*args, **kw)
        raise Exception("Unknown update_type : %s" % update_type)


    def _updateSquare(self, x, y, new_piece):
        """
        Updates the current game with the given information :
        The square located on x,y has a new piece
        """
        # change_history[-1] because we append the new change to everything
        current_repr = self.change_history[-1].board_after
        current_piece = current_repr[y][x]
        inserted_change = None
        if current_piece == new_piece:
            self.log(VERBOSE, "Piece on %s,%s is already %s - skipping", x, y, new_piece)
            return
        if current_piece != "." and new_piece != ".":
            # Insert a change to free the square
            inserted_repr = current_repr[:]
            row = list(inserted_repr[y])
            row[x] = "."
            inserted_repr[y] = "".join(row)
            inserted_change = PhysicalBoardChange(len(self.change_history), ".", current_piece, Coordinate(x,y), inserted_repr)
            self.log(VERBOSE, "Receiving from board a change without freeing the square. Inserting a freeing change %s" % inserted_change)
            self.change_history.append(inserted_change)
            current_piece = "."

        new_repr = current_repr[:]
        row = list(new_repr[y])
        row[x] = new_piece
        new_repr[y] = "".join(row)
        current_change = PhysicalBoardChange(len(self.change_history), new_piece, current_piece, Coordinate(x,y), new_repr)
        self.log(VERBOSE, "New change from board : %s", current_change)
        self.change_history.append(current_change)
        if self.recording:
            if inserted_change is not None:
                self.log(VERBOSE, "Recording is on, handling the inserted change first")
                self.change_buffer.append(inserted_change) # XXX Only if pending_changes is empty
                self._handleChange(inserted_change)
            self.log(VERBOSE, "Recording is on, handling the new change")
            self.change_buffer.append(current_change) # XXX Only if pending_changes is empty
            self._handleChange(current_change)

            # Test if change_buffer is getting too big
            if len(self.change_buffer) > MAX_BUFFERED_CHANGES:
                self.log(ERROR, "change_buffer is too big. Undoing last move")
                undo()
                raise NeedHelp("Change buffer is too big. Something was wrong. Undid last move")

        else:
            if inserted_change is not None:
                self.log(VERBOSE, "Recording is off, storing the inserted change in pending_changes")
                self.pending_changes.append(inserted_change)
            self.log(VERBOSE, "Recording is off, storing the new change in pending_changes")
            self.pending_changes.append(current_change)


    def _handleChange(self, current_change):
        """
        Internal method used to treat the content of the change buffer
        and find associated moves
        The buffer is populated for each new arriving change while recording is on
        If recording is off, the buffer is not populated anymore, but another
        buffer is filled. When recording is set on again, the other buffer
        empties into change_buffer and each moved change is analyzed here.
        """
        # XXX Manage King positions to indicate game result
        # Search the change in change_buffer and operate only until it
        change_buffer = None
        for i in range(len(self.change_buffer)):
            if self.change_buffer[i].index == current_change.index:
                change_buffer = self.change_buffer[:i+1]
        if change_buffer is None:
            raise Exception("Change %s is not in change_buffer" % current_change)
        self.log(VERBOSE, "Handling change %s" % current_change)
        new_piece = current_change.piece
        old_piece = current_change.old_piece

        kw = {
            "current_change": current_change,
            "change_buffer" : change_buffer,
            "new_piece" : new_piece,
            "old_piece" : old_piece
        }

        # First search for a pending symmetrical move (i.e. "square empty" <-> "square gets a piece")
        if self._searchSymmetricalChange(**kw):
            return

        # Search for an awaited change from the previous moves
        for last_move in self.move_history[-self.max_pending_moves:]:
            for change in last_move.getAwaitedChanges():
                if change == current_change:
                    self.log(VERBOSE, "Found awaited change '%s' from previous move '%s'", current_change, last_move)
                    last_move.associateChange(current_change)
                    last_move.removeAwaitedChange(current_change)
                    removeExactChange(self.change_buffer, current_change)
                    return
            # Intermediate changes cannot occur after the ending change of a move :
            # - Promotion is done on the ending square
            # - Capture is done on the ending square, excepting for en passant,
            #       but in this case the change related to the removed Pawn is mandatory
            #       and registered as an initial move

        if new_piece == ".":
            # Look for a previous ambiguous change and try to solve it
            # with the current change
            if self._searchUnambiguatingChange(**kw):
                return
            self.log(VERBOSE, "Nothing found for change '%s'. Waiting next changes", current_change)
            return

        # A landing piece can be the "key" change for a move
        if self._getPieceColour(new_piece) == self.chessboard.turn:
            # The previous move was correct and the current player now plays
            if self._findMove(**kw):
                return
            # _findMove always returns True

        # The landed is of the same colour as the last played move
        if len(self.move_history)>0 and self.move_history[-1].index > current_change.index:
            self.log(VERBOSE, "Change %s occurred before Move %s and cannot replace it. Waiting for next changes", current_change, self.move_history[-1])
            return

        if len(self.move_history) == 0:
            self.log(VERBOSE, "The wrong player plays, but there are no move to undo. Waiting for next changes")
            return

        # The opponent player plays again !
        # It means the previous identified move was not ended
        # We need to cancel it and reiterate from the previous validated position
        self._cancelAndReiterate(**kw)


    def _searchSymmetricalChange(self, **kw):
        """
        Internal method
        """
        current_change = kw["current_change"]
        change_buffer = kw["change_buffer"]
        new_piece = kw["new_piece"]
        old_piece = kw["old_piece"]
        symmetrical_change = None
        for change in change_buffer[:-1]:
            if symmetrical_change is None:
                if change.coord == current_change.coord and \
                   ((new_piece == "." and change.piece == old_piece) or \
                    (new_piece == change.old_piece and change.piece == ".")):
                        symmetrical_change = change
        if symmetrical_change is not None:
            self.log(VERBOSE, "Found symmetrical change '%s' for '%s' : cancelling both", symmetrical_change, current_change)
            current_change.cancel(symmetrical_change)
            removeExactChange(self.change_buffer, symmetrical_change)
            removeExactChange(self.change_buffer, current_change)
            return True
        return False


    def _searchUnambiguatingChange(self, **kw):
        """
        Internal method
        """
        current_change = kw["current_change"]
        ambiguous_change = self.last_ambiguous_change
        if ambiguous_change is None:
            return False
        # Try to associate this starting change with previous ambiguous one
        index = findExactChange(self.change_buffer, ambiguous_change)
        if index is not None and index < current_change.index:
            self.log(VERBOSE, "Trying to move the currently handled change %s to index %s of change_buffer to resolve ambiguous change %s", current_change, index, ambiguous_change)
            removeExactChange(self.change_buffer, current_change)
            self.change_buffer.insert(index, current_change)
            self._handleChange(ambiguous_change)
            if findExactChange(self.change_buffer, current_change) is None:
                self.log(VERBOSE, "Change %s resolved previous ambiguous change %s !", current_change, ambiguous_change)
                return True
            else:
                self.log(VERBOSE, "Change %s did not resold ambiguous change. Resorting change_buffer", current_change)
                self.change_buffer.sort(key=lambda c: c.index)
        return False

            
    def _findMove(self, **kw):
        """
        Internal method
        """
        current_change = kw["current_change"]
        change_buffer = kw["change_buffer"]
        candidate_list = CandidateMoves(self.chessboard).getCandidates(current_change)
        if len(candidate_list) == 0:
            self.log(VERBOSE, "No candidate move for change '%s'. Waiting next changes", current_change)
            return True
        if len(candidate_list) > 1:
            # There are more than one possible candidate.
            # We search for corresponding initial changes
            self.log(VERBOSE, "Candidate moves for change '%s' : %s. Searching initial changes",
                     current_change, ";".join([str(m) for m in candidate_list]))
            new_candidate_list = []
            for candidate in candidate_list:
                if len([c for c in change_buffer[:-1] if c in candidate.initial_changes]) > 0:
                    new_candidate_list.append(candidate)
            if len(new_candidate_list) == 0:
                self.log(VERBOSE, "No potential initial change found. Waiting next changes")
                self.last_ambiguous_change = current_change
                return True
            if len(new_candidate_list) > 1:
                self.log(VERBOSE, "Moves found for '%s' : %s. Need human help",
                         current_change, ";".join([str(m) for m in new_candidate_list]))
                raise NeedHelp("Moves found for '%s' : %s" % (current_change, ";".join([str(m) for m in new_candidate_list])))

            # Found a single move, so apply it
            self.log(VERBOSE, "Move selected for '%s' : %s. Applying", current_change, new_candidate_list[0])
            self._applyMove(new_candidate_list[0], current_change.index)
            return True

        # Change not ambiguous
        self.log(VERBOSE, "Only move found for '%s' : %s. Applying", current_change, candidate_list[0])
        self._applyMove(candidate_list[0], current_change.index)
        return True


    def _cancelAndReiterate(self, **kw):
        """
        Internal method
        """
        current_change = kw["current_change"]
        self.log(VERBOSE, "The same player plays again. It means the latest move was wrong. Undoing it")
        self.undo()
        self.record(True)
        self.log(INFO, "Last move was undone because the same player has played again. Restarting _handleBuffer")
        self._handleChange(current_change)
        if current_change in self.change_buffer:
            self.log(VERBOSE, "We did an undo for %s, but could not determine a move. Rehandling all changes in change_buffer", current_change)
        else:
            self.log(VERBOSE, "%s needed an undo to be interpreted, rehandling all changes in change_buffer", current_change)
        for change in self.change_buffer[:]:
            # We work on a copy
            if change in self.change_buffer and change.index < current_change.index:
                self._handleChange(change)
        self.log(VERBOSE, "Rehandled all changes in change_buffer. End of handling for %s", current_change)


    def _applyMove(self, move, index):
        """
        Apply the given move, associate changes with it, and record the move in ChessBoard
        """
        # In this method we do some sanity checks to avoid strange behaviours
        # Do not hesitate to call for human help : players are human, then can do anything !
        self.log(INFO, "Applying move %s...", move)

        # Search the change in change_buffer and operate only until it
        change_buffer = None
        for i in range(len(self.change_buffer)):
            if self.change_buffer[i].index == index:
                change_buffer = self.change_buffer[:i+1]
        if change_buffer is None:
            raise Exception("Change index %s is not in change_buffer" % index)

        # First verify if there are no changes waiting for a while
        try:
            old_move = self.move_history[-self.max_pending_moves]
            awaited_changes = old_move.getAwaitedChanges()
            if len(awaited_changes) > 0:
                self.log(ERROR, "Still waiting changes '%s' for move '%s'. Something is wrong",
                         ";".join([str(c) for c in awaited_changes]), old_move)
                for i in range(self.max_pending_moves):
                    self.undo()
                raise NeedHelp("Still waiting changes '%s' for move '%s'. Undid %s last moves " % (
                               ";".join([str(c) for c in awaited_changes]), old_move, self.max_pending_moves))
        except IndexError:
            # Simply no old moves
            old_move = None

        # Apply the move itself and associate it with changes
        move.setIndex(index)
        self.chessboard.makeMove(move.short_notation)
        awaited_changes = set(move.initial_changes) | set(move.final_changes)
        intermediate_changes = set(move.intermediate_changes)
        for change in change_buffer:
            if change in awaited_changes:
                self.log(VERBOSE, "Consuming mandatory change %s", change)
                awaited_changes.remove(change)
                move.associateChange(change)
                removeExactChange(self.change_buffer, change)
            elif change in intermediate_changes:
                self.log(VERBOSE, "Consuming intermediate change %s", change)
                move.associateChange(change)
                removeExactChange(self.change_buffer, change)
            else:
                self.log(VERBOSE, "Change %s is independent from move %s", change, move)
        self.log(VERBOSE, "Still awaited changes after consuming existing ones : %s", ";".join([str(c) for c in awaited_changes]))
        move.setAwaitedChanges(*awaited_changes)
        self.move_history.append(move)

        # Finally check if some changes in buffer are too old
        for change in self.change_buffer:
            change.age += 1
            if change.age >= self.max_pending_moves:
                self.log(ERROR, "Change '%s' is too old. Something is wrong", change)
                for i in range(self.max_pending_moves):
                    self.undo()
                raise NeedHelp("Change '%s' is too old. Something is wrong. " % (change) +
                               "Undid %s last moves" % self.max_pending_moves)


    def undo(self):
        """
        Undo the last move, replace the associated changes in the buffer,
        and stop the recording to wait a human intervention
        """
        if len(self.move_history) == 0:
            # XXX exception ?
            return
        self.record(False)
        last_move = self.move_history.pop()
        for change in self.change_buffer:
            change.age -= 1
        self.log(INFO, "Undoing move %s", last_move)
        unassociated_changes = last_move.associated_changes
        index = last_move.index
        for change in unassociated_changes:
            self.log(VERBOSE, "Unassociating change %s", change)
            change.unassociate()
            #if change.index > index:
            #    self.log(VERBOSE, "Change %s is placed in pending_changes", change)
            #    self.pending_changes.append(change)
            #else:
            self.log(VERBOSE, "Change %s is placed in change_buffer", change)
            self.change_buffer.append(change)
        #for change in self.change_buffer[:]:
        #    if change.index > index:
        #        self.log(VERBOSE, "Moving change %s from change_buffer to pending_change", change)
        #        self.pending_changes.append(change)
        #        self.change_buffer.remove(change)
        #self.pending_changes.sort(key=lambda c: c.index)
        self.change_buffer.sort(key=lambda c: c.index)
        self.chessboard.undo()
        self.log(INFO, "Move %s is undone", last_move)


        
if __name__ == "__main__":
    print "-- Test 1 : Build a 8x8 ChessGame and try simple moves"
    game = ChessGame(8,8)
    game.record(True)
    game.name = "Test1-1"
    # Remove and replace a piece
    game.update("square", 3, 0, ".")
    assert len(game.change_buffer) == 1
    assert len(game.change_history) == 2 # There is an empty change at the beginning
    game.update("square", 3, 0, "Q")
    assert len(game.change_buffer) == 0
    assert len(game.change_history) == 3
    # Remove a piece and replace it with another (strange move)
    game.update("square", 3, 0, ".")
    game.update("square", 3, 0, "N")
    assert len(game.change_buffer) == 2
    assert len(game.change_history) == 5

    # Simple move : remove a piece from a square and deposit it elsewhere
    game = ChessGame(8,8)
    game.record(True)
    game.name = "Test1-2"
    game.update("square", 4, 1, ".")
    game.update("square", 4, 3, "P")
    assert len(game.change_buffer) == 0
    assert len(game.change_history) == 3
    assert game.move_history[-1].short_notation == "e4"

    # Idem but the deposit is detected before the remove
    game.name = "Test1-3"
    game.update("square", 4, 4, "p")
    game.update("square", 4, 6, ".")
    assert len(game.change_buffer) == 0
    assert len(game.change_history) == 5
    assert game.move_history[-1].short_notation == "e5"

    # Idem but the Pawn goes too far, then it is replaced on the good square
    game.name = "Test1-4"
    game.update("square", 3, 4, "P")
    game.update("square", 3, 3, "P")
    game.update("square", 3, 1, ".")
    game.update("square", 3, 4, ".")
    assert len(game.change_buffer) == 0
    assert len(game.change_history) == 9
    assert game.move_history[-1].short_notation == "d4"

    # Simple capture
    game.name = "Test1-5"
    game.update("square", 4, 4, ".")
    game.update("square", 3, 3, "p")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "exd4"

    # More complex capture
    game.name = "Test1-6"
    game.update("square", 3, 3, ".")
    game.update("square", 2, 3, "Q") # The Queen deviates a little
    game.update("square", 3, 0, ".")
    game.update("square", 3, 3, "Q")
    game.update("square", 2, 3, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "Qxd4"


    print "\n\n-- Test 2 : Moves with intermediate landing squares"
    game.name = "Test2-1"
    game.update("square", 3, 5, "b")
    game.update("square", 5, 7, ".")
    game.update("square", 2, 4, "b")
    game.update("square", 3, 5, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "Bc5"

    game.name = "Test2-2"
    game.update("square", 5, 3, "B")
    game.update("square", 2, 0, ".")
    game.update("square", 5, 3, ".")
    game.update("square", 4, 2, "B")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "Be3"


    print "\n\n-- Test 3 : Illegal landing moves for the same colour of the last move"
    # Illegal land after a move
    game.name = "Test3-1"
    game.update("square", 0, 6, ".")
    game.update("square", 0, 4, "p")
    game.update("square", 0, 3, "p")
    game.update("square", 0, 3, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "a5"

    # Complex illegal changes
    game.name = "Test3-2"
    game.update("square", 7, 1, ".")
    game.update("square", 7, 3, "P")
    game.update("square", 7, 4, "P")
    game.update("square", 7, 5, "P")
    game.update("square", 7, 3, ".")
    game.update("square", 7, 4, ".")
    game.update("square", 7, 5, ".")
    game.update("square", 7, 3, "P")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "h4"


    print "\n\n-- Test 4 : Move, then undo it and make another"
    game.name = "Test4-1"
    game.update("square", 5, 5, "q")
    game.update("square", 3, 7, ".")
    game.update("square", 3, 7, "q")
    game.update("square", 5, 5, ".")
    # In this case the move is not undone until next legal move
    # from the same player (it is due to implementation)
    assert len(game.change_buffer) == 1
    assert game.move_history[-1].short_notation == "Qf6"
    game.update("square", 4, 6, "q")
    game.update("square", 3, 7, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "Qe7"
    game.name = "Test4-2"
    # Undo Queen move and finally move a Pawn
    game.update("square", 4, 6, ".")
    game.update("square", 3, 7, "q")
    game.update("square", 1, 6, ".")
    game.update("square", 1, 5, "p")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "b6"


    print "\n\n-- Test 5 : Intensive forward-backward from a piece"
    game.name = "Test5-1"
    game.update("square", 1, 3, "Q")
    game.update("square", 0, 3, "Q")
    game.update("square", 3, 3, ".")
    game.update("square", 2, 3, "Q")
    game.update("square", 0, 3, ".")
    game.update("square", 1, 3, ".")
    game.update("square", 1, 3, "Q")
    game.update("square", 2, 3, ".")
    game.update("square", 1, 3, ".")
    game.update("square", 0, 3, "Q")
    game.update("square", 1, 3, "Q")
    game.update("square", 2, 3, "Q")
    game.update("square", 3, 3, "Q")
    game.update("square", 0, 3, ".")
    game.update("square", 1, 3, ".")
    game.update("square", 2, 3, ".")
    # In this case the move is not undone until next legal move
    # from the same player (it is due to implementation)
    assert len(game.change_buffer) == 1
    assert game.move_history[-1].short_notation == "Qc4"


    print "\n\n-- Test 6 : DGT extreme situation example as described in dgtbrd.h"
    game.name = "Test6-1"
    game.update("square", 3, 6, ".")
    game.update("square", 3, 4, "Q")
    game.update("square", 3, 5, "Q")
    game.update("square", 3, 3, ".")
    game.update("square", 3, 4, ".")
    game.update("square", 3, 6, "Q")
    game.update("square", 3, 5, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "Qxd7+"
    # Replace in a similar position (... Kf8 Qg4 c6)
    game.name = "Test6-2"
    game.update("square", 4, 7, ".")
    game.update("square", 5, 7, "k")
    game.update("square", 3, 6, ".")
    game.update("square", 6, 3, "Q")
    game.update("square", 2, 6, ".")
    game.update("square", 2, 5, "p")
    # Same DGT extreme situation, but we suppose board scan is reverted
    game.name = "Test6-3"
    game.update("square", 6, 6, ".")
    game.update("square", 6, 4, "Q")
    game.update("square", 6, 3, ".")
    game.update("square", 6, 6, "Q")
    game.update("square", 6, 4, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "Qxg7+"
    saved_game = game


    print "\n\n-- Test 7 : Promotion"
    promote_position = "r..r..../k...P.../......../......../......../......../......../....K..."
    # Simple case : move the Pawn, remove it, place a Queen
    game = ChessGame(8,8, initial_position=promote_position)
    game.record(True)
    game.name = "Test7-1"
    game.update("square", 4, 6, ".")
    game.update("square", 4, 7, "P")
    game.update("square", 4, 7, ".")
    game.update("square", 4, 7, "Q")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "e8=Q"
    # Idem with another sequence
    game = ChessGame(8,8, initial_position=promote_position)
    game.record(True)
    game.name = "Test7-2"
    game.update("square", 4, 7, "P")
    game.update("square", 4, 6, ".")
    game.update("square", 4, 7, ".")
    game.update("square", 4, 7, "Q")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "e8=Q"
    # Most common case : remove the Pawn, place a Queen
    game = ChessGame(8,8, initial_position=promote_position)
    game.record(True)
    game.name = "Test7-3"
    game.update("square", 4, 6, ".")
    game.update("square", 4, 7, "Q")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "e8=Q"
    # Second most common case : place a Queen, then remove the Pawn
    game = ChessGame(8,8, initial_position=promote_position)
    game.record(True)
    game.name = "Test7-4"
    game.update("square", 4, 7, "Q")
    game.update("square", 4, 6, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "e8=Q"
    # Simple case of taking the Rook
    game = ChessGame(8,8, initial_position=promote_position)
    game.record(True)
    game.name = "Test7-5"
    game.update("square", 4, 6, ".")
    game.update("square", 3, 7, "P")
    game.update("square", 3, 7, "Q")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "exd8=Q"
    # More complex case of taking the Rook
    game = ChessGame(8,8, initial_position=promote_position)
    game.record(True)
    game.name = "Test7-6"
    game.update("square", 3, 7, ".")
    game.update("square", 4, 6, ".")
    game.update("square", 3, 7, "Q")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "exd8=Q"


    print "\n\n-- Test 8 : Castling"
    castle_position = "r...k..r/......../......../......../......../......../......../R...K..R"
    # Simple case : Kingside castling with rapid standard move
    game = ChessGame(8,8, initial_position=castle_position)
    game.record(True)
    game.name = "Test8-1"
    game.update("square", 6, 0, "K")
    game.update("square", 4, 0, ".")
    game.update("square", 5, 0, "R")
    game.update("square", 7, 0, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "O-O"
    # Kingside castling with intermediate changes
    game = ChessGame(8,8, initial_position=castle_position)
    game.record(True)
    game.name = "Test8-2"
    game.update("square", 5, 0, "K")
    game.update("square", 4, 0, ".")
    game.update("square", 6, 0, "K")
    game.update("square", 7, 0, ".")
    game.update("square", 5, 0, ".")
    game.update("square", 5, 0, "R")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "O-O"
    # Same without freeing f1 after King passage
    game = ChessGame(8,8, initial_position=castle_position)
    game.record(True)
    game.name = "Test8-3"
    game.update("square", 5, 0, "K")
    game.update("square", 4, 0, ".")
    game.update("square", 6, 0, "K")
    game.update("square", 7, 0, ".")
    game.update("square", 5, 0, "R")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "O-O"
    # Move Rook first (forbidden but try anyway...)
    game = ChessGame(8,8, initial_position=castle_position)
    game.record(True)
    game.name = "Test8-4"
    game.update("square", 6, 0, "R")
    game.update("square", 5, 0, "R")
    game.update("square", 6, 0, "K")
    game.update("square", 7, 0, ".")
    game.update("square", 4, 0, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "O-O"
    # Queenside complex castling (in some cases it needs human help)
    game = ChessGame(8,8, initial_position=castle_position)
    game.record(True)
    game.name = "Test8-5"
    game.update("square", 3, 0, "K")
    game.update("square", 2, 0, "K")
    game.update("square", 3, 0, "R")
    game.update("square", 4, 0, ".")
    game.update("square", 0, 0, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "O-O-O"


    print "\n\n-- Test 9 : En passant"
    enpassant_position = "k......./.....p../......../......../....P.../......../......../....K..."
    # Most common case : move the white Pawn, then remove black one
    game = ChessGame(8,8, initial_position=enpassant_position)
    game.record(True)
    game.name = "Test9-1"
    game.update("square", 4, 3, ".")
    game.update("square", 4, 4, "P")
    game.update("square", 5, 6, ".")
    game.update("square", 5, 4, "p")
    # Preparation done, now the tested moves
    game.update("square", 4, 4, ".")
    game.update("square", 5, 5, "P")
    game.update("square", 5, 4, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "exf6"
    # Another common case : remove the black Pawn, then move white one
    game = ChessGame(8,8, initial_position=enpassant_position)
    game.record(True)
    game.name = "Test9-2"
    game.update("square", 4, 3, ".")
    game.update("square", 4, 4, "P")
    game.update("square", 5, 6, ".")
    game.update("square", 5, 4, "p")
    # Preparation done, now the tested moves
    game.update("square", 5, 4, ".")
    game.update("square", 5, 5, "P")
    game.update("square", 4, 4, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "exf6"
    # A last test to be sure
    game = ChessGame(8,8, initial_position=enpassant_position)
    game.record(True)
    game.name = "Test9-3"
    game.update("square", 4, 3, ".")
    game.update("square", 4, 4, "P")
    game.update("square", 5, 6, ".")
    game.update("square", 5, 4, "p")
    # Preparation done, now the tested moves
    game.update("square", 5, 5, "P")
    game.update("square", 5, 4, ".")
    game.update("square", 4, 4, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "exf6"


    print "\n\n-- Test 10 : Ambiguous move"
    ambiguous_position = "......../....P.N./......../......../......../......../......../k...K..."
    # Most simple case
    game = ChessGame(8,8, initial_position=ambiguous_position)
    game.record(True)
    game.name = "Test10-1"
    game.update("square", 4, 6, ".")
    game.update("square", 4, 7, "N")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "e8=N"
    # Inverted changes
    game = ChessGame(8,8, initial_position=ambiguous_position)
    game.record(True)
    game.name = "Test10-2"
    game.update("square", 4, 7, "N")
    game.update("square", 4, 6, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "e8=N"


    print "\n\n-- Test 11 : Situations needing human intervention (or not)"
    game = saved_game
    game.name = "Test11-1"
    game.update("square", 5, 7, ".")
    game.update("square", 4, 7, "k")
    game.update("square", 6, 6, ".")
    game.update("square", 3, 3, "Q")
    # Previous moves were just preparation
    # Now testing a capture (BxQ), then the player undoes it
    game.update("square", 2, 4, ".")
    # The player hesitates...
    game.update("square", 3, 3, ".")
    game.update("square", 3, 3, "Q")
    game.update("square", 2, 4, "b")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "Qd4"
    # ... then capture really
    game.update("square", 3, 3, ".")
    game.update("square", 3, 3, "b")
    game.update("square", 2, 4, ".")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "Bxd4"
    # Undo the capture simply
    game.update("square", 3, 3, ".")
    game.update("square", 2, 4, "b")
    game.update("square", 3, 3, "Q")
    # Then make some valid moves
    game.update("square", 0, 1, ".")
    game.update("square", 0, 2, "P")
    game.update("square", 7, 6, ".")
    game.update("square", 7, 5, "p")
    game.update("square", 0, 2, ".")
    try:
        game.update("square", 0, 3, "P")
    except NeedHelp, e:
        print "OK: Found a NeedHelp exception '%s'" % " ".join([str(a) for a in e.args])
        assert len(game.change_buffer) == 10
        assert game.move_history[-1].short_notation == "Qd4"
    else:
        assert False, "No NeedHelp exception raised"

    # A Pawn moves too far, then go back on its real destination
    game = ChessGame(8,8)
    game.record(True)
    game.name = "Test11-2"
    game.update("square", 0, 1, ".")
    game.update("square", 0, 3, "P")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "a4"
    # This order does not raise NeedHelp
    game.update("square", 0, 6, ".")
    game.update("square", 0, 3, "p")
    game.update("square", 0, 4, "p")
    game.update("square", 0, 3, "P")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "a5"
    game.name = "Test11-3"
    game.update("square", 1, 1, ".")
    game.update("square", 1, 3, "P")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "b4"
    # Try another order - still no raise
    game.update("square", 1, 5, "p")
    game.update("square", 1, 4, "p")
    game.update("square", 1, 3, "p")
    game.update("square", 1, 6, ".")
    game.update("square", 1, 5, ".")
    game.update("square", 1, 4, ".")
    game.update("square", 1, 3, "P")
    game.update("square", 1, 4, "p")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "b5"
    game.name = "Test11-4"
    game.update("square", 2, 1, ".")
    game.update("square", 2, 3, "P")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "c4"
    # Still another order
    game.update("square", 2, 4, "p")
    game.update("square", 2, 3, "p")
    game.update("square", 2, 6, ".")
    game.update("square", 2, 4, ".")
    game.update("square", 2, 4, "p")
    game.update("square", 2, 3, "P")
    assert len(game.change_buffer) == 0
    assert game.move_history[-1].short_notation == "c5"



    # XXX King position for end
    # XXX Human decisions
    # XXX Too many pending changes

    print "\n\nOK ! All test passed successful."


