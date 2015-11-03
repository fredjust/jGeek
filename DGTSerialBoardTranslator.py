#!/usr/bin/python
# -*- coding: utf-8 -*-

#######################################################################
# DGTSerialBoardTranslator.py - Copyright 2011 Guillaume MICHON
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


from serial import *
from time import sleep


# XXX Should be in another file
DEBUG = "DEBUG"
VERBOSE = "VERBOSE"
INFO = "INFO"
WARNING = "WARNING"
ERROR = "ERROR"
FATAL = "FATAL"


##### Data from dgtbrd.h #####
DGT_MESSAGE_BIT =        0x80
### Bus mode commands:
DGT_BUS_SEND_CLK =       0x01 | DGT_MESSAGE_BIT
DGT_BUS_SEND_BRD =       0x02 | DGT_MESSAGE_BIT
DGT_BUS_SEND_CHANGES =   0x03 | DGT_MESSAGE_BIT
DGT_BUS_REPEAT_CHANGES = 0x04 | DGT_MESSAGE_BIT
DGT_BUS_SET_START_GAME = 0x05 | DGT_MESSAGE_BIT
DGT_BUS_SEND_FROM_START= 0x06 | DGT_MESSAGE_BIT
DGT_BUS_PING =           0x07 | DGT_MESSAGE_BIT
DGT_BUS_END_BUSMODE =    0x08 | DGT_MESSAGE_BIT
DGT_BUS_RESET =          0x09 | DGT_MESSAGE_BIT
DGT_BUS_IGNORE_NEXT_BUS_PING = 0x0a | DGT_MESSAGE_BIT
DGT_BUS_SEND_VERSION =   0x0b | DGT_MESSAGE_BIT
DGT_BUS_SEND_BRD_50B =   0x0c | DGT_MESSAGE_BIT
# sends consequitively: CHANGES, CLOCK, BOARD:
DGT_BUS_SEND_ALL_D =     0x0d | DGT_MESSAGE_BIT

# Responses
DGT_MSG_BUS_BRD_DUMP =   0x03 | DGT_MESSAGE_BIT
DGT_MSG_BUS_BWTIME =     0x04 | DGT_MESSAGE_BIT
DGT_MSG_BUS_UPDATE =     0x05 | DGT_MESSAGE_BIT
DGT_MSG_BUS_FROM_START=  0x06 | DGT_MESSAGE_BIT
DGT_MSG_BUS_PING =       0x07 | DGT_MESSAGE_BIT
DGT_MSG_BUS_START_GAME_WRITTEN = 0x08 | DGT_MESSAGE_BIT
DGT_MSG_BUS_VERSION =    0x09 | DGT_MESSAGE_BIT
DGT_MSG_BUS_BRD_DUMP_50B=0x0a | DGT_MESSAGE_BIT
KNOWN_MESSAGES = [DGT_MSG_BUS_BRD_DUMP, DGT_MSG_BUS_BWTIME, DGT_MSG_BUS_UPDATE,
      DGT_MSG_BUS_FROM_START, DGT_MSG_BUS_PING, DGT_MSG_BUS_START_GAME_WRITTEN,
      DGT_MSG_BUS_VERSION, DGT_MSG_BUS_BRD_DUMP_50B]

# Piece codes for chess pieces
PIECE_CODE = [".","P","R","N","B","K","Q","p","r","n","b","k","q","U","U","U"]
# (the last 3 are reserved for future use)Not used

# Definition of the one-byte EEPROM message codes
EE_POWERUP              = 0x6a
EE_EOF                  = 0x6b
EE_FOURROWS             = 0x6c
EE_EMPTYBOARD           = 0x6d
EE_DOWNLOADED           = 0x6e
EE_BEGINPOS             = 0x6f
EE_BEGINPOS_ROT         = 0x7a
EE_START_TAG            = 0x7b
EE_WATCHDOG_ACTION      = 0x7c
EE_FUTURE_1             = 0x7d
EE_FUTURE_2             = 0x7e
EE_NOP                  = 0x7f


##### Addendum #####
CLOCK_WHITE = "white"
CLOCK_BLACK = "black"
CLOCK_LEFT = CLOCK_BLACK
CLOCK_RIGHT = CLOCK_WHITE




def byteToHex(byte_str):
    return ''.join( [ "%02X " % x for x in byte_str ] ).strip()


class HistoryError(Exception):
    """
    Raised when a DGT_MSG_BUS_UPDATE returns wrong data
    """
    pass


class BoardMessage(object):
    """
    A serial message from a board in bus mode
    """
    def __init__(self):
        self.buf = bytearray()
        self.msg_type = None
        self.length = 0
        self.board_address = None   # Raw board address (not translated in 8-bit)
        self.data = None
        self.check_ok = False

    def append(self, data):
        """
        Start or continue to construct the current message
        with given data from serial connection.
        If the message is complete, return the remaining data
        (not comsumpted)
        If the message is not started and some strange data
        is given before, return rejected bytes.
        Returns : (rejected, remaining)
        """
        rejected = bytearray()
        if self.length == 0:
            while len(self.buf) == 0 and len(data) != 0:
                self.buf.append(data.pop(0))
                if not self.buf[0] in KNOWN_MESSAGES:
                    rejected.append(self.buf.pop(0))
            needed_bytes = 3 - len(self.buf)
            for i in range(min(needed_bytes, len(data))):
                self.buf.append(data.pop(0))
                if not self.buf[0] in KNOWN_MESSAGES:
                    rejected.append(self.buf.pop(0))
            if len(self.buf) >= 3:
                self._getSize()
            else:
                return (bytearray(), bytearray())
        copy_size = min(self.length - len(self.buf), len(data))
        self.buf += data[:copy_size]
        if len(self.buf) == self.length:
            self._finish()
        return (rejected, data[copy_size:])

    def _getSize(self):
        """
        Determine the size of the message according to its header
        """
        ## From dgtbrd.h :
        #byte 2: message length MSB (from byte 1 to checksum) (D7=0)
        #byte 3: message length LSB (containing D0 to D6 of the length (D7 = 0)

        ## From elsewhere in dgtbrd.h :
        #/* Macros for message length coding (to avoid MSB set to 1) */
        #define BYTE    char
        #define LLL_SEVEN(a) ((BYTE)(a&0x7f))            /* 0000 0000 0111 1111 */
        #define LLH_SEVEN(a) ((BYTE)((a & 0x3F80)>>7))   /* 0011 1111 1000 0000 */
        ## -> Here, LLH_SEVEN -> Byte 2 ; LLL_SEVEN -> Byte 3
        self.length = int(self.buf[1]<<7) + int(self.buf[2])

    def _finish(self):
        """
        Determine the board address and verify checksum
        """
        ## From dgtbrd.h :
        #The message format is:
        #byte 1: message type byte (MSB = 1)
        #byte 4: board address MSB (D7=0)
        #byte 5: board address LSB (D7=0)
        # < data bytes: 0 to theoretically 16K-6 bytes >
        #last byte: checksum. This is the sum of all byte values from byte 1 to
        #       the byte before this checksum. D7 is forced to 0
        self.msg_type = self.buf[0]
        self.board_address = self.buf[3:5]
        self.data = self.buf[5:-1]
        checksum = (sum(self.buf[:-1]) & ~0x80) % 256
        if checksum == self.buf[-1]:
            self.check_ok = True

    def getState(self):
        """
        Return the state :
            * "waiting" : Waiting for data
            * "complete" : The message is complete and verified
            * "error" : Checksum error
        """
        if self.length == 0 or len(self.buf) != self.length:
            return "waiting"
        return self.check_ok and "complete" or "error"


class BoardTranslator(object):
    """
    This class manages the low level communication with DGT Serial Boards
    (in bus mode) and provides a higher level API for software
    ! Not thread-safe
    """
    def __init__(self, log=None):
        self.log_function = log or self.defaultLogFunction
        self.boards = {}
        self._scanPorts()


    def log(self, level, message, *args):
        """
        Logs a message
        """
        self.log_function("DGTSerial", level, message, *args)
    def defaultLogFunction(self, obj, level, message, *args):
        """
        Default log function : it simply prints the message on stdout
        """
        print "[%s] - %s - %s" % (obj, level, message % args)


    def _scanPorts(self):
        """
        Scan for available ports and stores the found list.
        Ports are closed after operation !
        """
        self.log(INFO, "Scanning serial ports")
        self.ports = {}
        for i in range(256):
            try:
                s = Serial(i)
                self.ports[s.portstr] = {"id":i, "handler":None, "name":s.portstr}
                s.close()
            except SerialException:
                pass
        self.log(INFO, "Available ports : %s", ";".join(self.ports.keys()))


    def getAvailablePorts(self):
        """
        Returns a list of the available serial ports on
        the computer, as string representations
        """
        return self.ports.keys()


    def usePort(self, port, use=True):
        """
        Set the given port to be used for communication or not
        port is a string
        """
        try:
            port_dict = self.ports[port]
        except KeyError:
            raise Exception("Port %s does not exist !" % port)

        if port_dict["handler"] is None:
            if use:
                self._openPort(port)
            else:
                self.log(VERBOSE, "Port %s is already closed", port)
        else:
            if use:
                self.log(VERBOSE, "Port %s is already open", port)
            else:
                port_dict["handler"].close()
                port_dict["handler"] = None


    def _openPort(self, port):
        """
        Open the port with correct parameters
        """
        self.log(INFO, "Opening port %s" % port)
        self.ports[port]["handler"] = Serial(
            port = port,
            baudrate = 9600,
            bytesize = EIGHTBITS,
            parity = PARITY_NONE,
            stopbits = STOPBITS_ONE,
            timeout = 0.1,   # Arbitrary
	    xonxoff = False,
	    rtscts = False,
	    dsrdtr = False
        )
        # VERY IMPORTANT - unless the behaviour is very strange
        self.ports[port]["handler"].setDTR(0)
        self.ports[port]["handler"].setRTS(0)
        # First send a BUS_RESET and wait a few seconds to start cleanly
        self._sendOrder(bytearray([DGT_BUS_RESET,0,0]), self.ports[port])
        sleep(3)


    def discoverBoards(self):
        """
        Send an order to already discovered boards to ignore next ping,
        then send a ping to all boards to discover them
        """
        self.log(VERBOSE, "Discovering board")
        for board in self.boards.keys():
            self.log(VERBOSE, "Send IGNORE_NEXT_BUS_PING to board %s", board)
            self._communicateWithBoard(board, DGT_BUS_IGNORE_NEXT_BUS_PING, DGT_MSG_BUS_PING)
        self.log(VERBOSE, "Sending a BUS_PING to all boards")
        self._sendOrder(bytearray([DGT_BUS_PING,0,0]))
        sleep(1.5) # Arbitrary. Given time in dgtbrd.h is 1100 ms
        for port in [p for p in self.ports.values() if p["handler"] is not None]:
            self.log(VERBOSE, "Receiving ping responses on port %s", port["name"])
            try:
                for message in self._receiveMessage(port):
                    if message.msg_type == DGT_MSG_BUS_PING:
                        board_bus_address = message.board_address
                        ## From dgtbrd.h :
                        # Busadres in 2 bytes of 7 bits hexadecimal value
                        # byte : 0bbb bbbb with bus adres MSB 7 bits
                        # byte : 0bbb bbbb with bus adres LSB 7 bits
                        # The value of the 14-bit busadres is het hexadecimal representation
                        # of the (decimal coded) serial number
                        # i.e. When the serial number is "01025 1.0" the busadres will be
                        #      byte : 0000 1000 (0x08)
                        #      byte : 0000 0001 (0x01)
                        serial_number = int(board_bus_address[0]<<7) + int(board_bus_address[1])
                        if self.boards.has_key(serial_number):
                            self.log(WARNING, "A already known board responded to ping")
                        else:
                            self.log(INFO, "Discovered board S/N %s on port %s", serial_number, port["name"])
                            self.boards[serial_number] = {
                                "serial_number" : serial_number,
                                "bus_address" : board_bus_address,
                                "name" : "%s" % serial_number,
                                "port_name" : port["name"],
                                "history" : [],
                                "send_from_start" : False
                            }
                    else:
                        self.log(WARNING, "Received an unwanted message as ping response")
            except ChecksumError, e:
                self.log(ERROR, "ChecksumError on port %s : %s", port["name"], " ".join([str(a) for a in e.args]))


    def getBoards(self):
        """
        Return the list of the known boards, as tuples :
            (board S/N (int), serial port name)
        """
        return [(b["serial_number"], b["port_name"]) for b in self.boards.values()]


    def getBoardPosition(self, board):
        """
        Return the physical position on the given board.
        The return string is a dotted fen notation (None if error).
        """
        self.log(VERBOSE, "Send DGT_BUS_SEND_BRD to board %s", board)
        message = self._communicateWithBoard(board, DGT_BUS_SEND_BRD, DGT_MSG_BUS_BRD_DUMP)
        if message is None:
            return None
        row_list = []
        try:
            for row in range(8):
                row_list.append("".join([PIECE_CODE[d] for d in message.data[row*8:row*8+8]]))
        except (KeyError, IndexError):
            pass
        if len(row_list) == 8:
            return "/".join(row_list)
        self.log(WARNING, "Request for position : the board returned wrong data")


    def getBoardClock(self, board):
        """
        Return the clock timers for both player on board (None if error)
        Return dict format :
            { "white"|"black" : [hours, minutes, seconds],
              "turn" : "white|black",
              "present" : True/False indicating a clock is present or not
            }
        """
        self.log(VERBOSE, "Send DGT_BUS_SEND_CLK to board %s", board)
        message = self._communicateWithBoard(board, DGT_BUS_SEND_CLK, DGT_MSG_BUS_BWTIME)
        ## From dgtbrd.h :
        # byte 3:
        # D4: 1 = Flag fallen for left player, and clock blocked to zero
        #     0 = not the above situation
        # D5: 1 = Time per move indicator on for left player ( i.e. Bronstein, Fischer)
        #     0 = Time per move indicator off for left player
        # D6: 1 = Left players flag fallen and indicated on display
        #     0 = not the above situation
        # (D7 is MSB)
        # D0-D3: Hours (units, 0-9 Binary coded) white player (or player at the A side of the board)
        # byte 4: Minutes (0-59, BCD coded)
        # byte 5: Seconds (0-59, BCD coded)
        # byte 6-8: the same for the other player
        # byte 9: Clock status byte: 7 bits
        # D0 (LSB): 1 = Clock running
        #           0 = Clock stopped by Start/Stop
        # D1: 1 = tumbler position high on (white) player (front view: \ , left side high)
        #     0 = tumbler position high on the other player (front view: /, right side high)
        # D2: 1 = Battery low indication on display
        #     0 = no battery low indication on display
        # D3: 1 = Black players turn
        #     0 = not black players turn
        # D4: 1 = White players turn
        #     0 = not white players turn
        # D5: 1 = No clock connected; reading invalid
        #     0 = clock connected, reading valid
        # D6: not used (read as 0)
        # D7:  Always 0
        if message is None:
            return None
        return_dict = {"white": [], "black": [], "turn":None, "present":False}
        if message.data[6] & 0x20:
            # No clock present (Byte 9 - D5)
            return return_dict
        return_dict["present"] = True
        if message.data[6] & 0x02:
            return_dict["turn"] = CLOCK_LEFT
        else:
            return_dict["turn"] = CLOCK_RIGHT
        for player, data in [(CLOCK_LEFT, message.data[0:3]), (CLOCK_RIGHT, message.data[3:6])]:
            return_dict[player].append( data[0] & 0x0F )  # Hours
            return_dict[player].append( ((data[1] & 0xF0)>>4)*10 + (data[1] & 0x0F) ) # Minutes
            return_dict[player].append( ((data[2] & 0xF0)>>4)*10 + (data[2] & 0x0F) ) # Seconds
        return return_dict


    def setBoardStart(self, board):
        """
        Indicate to the board that it is its starting position. Only
        future moves will be returned.
        """
        self.log(VERBOSE, "Send DGT_BUS_SEND_CHANGES to board %s to reset it", board)
        message = self._communicateWithBoard(board, DGT_BUS_SEND_CHANGES, DGT_MSG_BUS_UPDATE)
        if message is None:
            raise Exception("setBoardStart : board %s did not answer" % board)
        self.log(VERBOSE, "Send DGT_BUS_SET_START_GAME to board %s", board)
        message = self._communicateWithBoard(board, DGT_BUS_SET_START_GAME, DGT_MSG_BUS_START_GAME_WRITTEN)
        if message is None:
            raise Exception("setBoardStart : board %s did not answer" % board)
        self.boards[board]["history"] = []


    def getBoardHistory(self, board, history_from=0):
        """
        Return all changes recorded in board memory, from history_from index (included)
        Return None if something goes wrong.
        """
        if self.boards[board]["send_from_start"]:
            if self._resetBoardHistory(board):
                return self.boards[board]["history"][history_from:]
            return None

        try:
            self.log(VERBOSE, "Send DGT_BUS_SEND_CHANGES to board %s", board)
            message = self._communicateWithBoard(board, DGT_BUS_SEND_CHANGES, DGT_MSG_BUS_UPDATE, tries=1)
        except ChecksumError:
            try:
                self.log(VERBOSE, "Send DGT_BUS_REPEAT_CHANGES to board %s", board)
                message = self._communicateWithBoard(board, DGT_BUS_REPEAT_CHANGES, DGT_MSG_BUS_UPDATE, tries=1)
            except ChecksumError:
                self.log(ERROR, "Two consecutive ChecksumError on board %s, delaying...", board)
                self.boards[board]["history"] = []
                self.boards[board]["send_from_start"] = True
                return None
        if message is None:
            self.log(WARNING, "getBoardHistory : board %s did not answer")
            # In this case, we don't know if the board wrote e EE_DOWNLOADED in its memory,
            # so we ask the history from start
            self.boards[board]["history"] = []
            self.boards[board]["send_from_start"] = True
            return None
        try:
            self._updateHistory(board, message.data)
        except HistoryError, e:
            self.log(ERROR, " ".join([str(a) for a in e.args]))
            self.boards[board]["history"] = []
            self.boards[board]["send_from_start"] = True
            return None
        return self.boards[board]["history"][history_from:]


    def _resetBoardHistory(self, board):
        """
        Reset the history for the given board, and ask its memory from the start
        Return True if the history is correctly reset, False else.
        """
        # XXX Need to test if a EE_DOWNLOADED is written when DGT_BUS_SEND_FROM_START is sent
        self.log(VERBOSE, "Send DGT_BUS_SEND_FROM_START to board %s", board)
        message = self._communicateWithBoard(board, DGT_BUS_SEND_FROM_START, DGT_MSG_BUS_FROM_START)
        if message is None:
            self.log(ERROR, "Board %s does not answer, delaying...", board)
            return False
        self.boards[board]["send_from_start"] = False
        try:
            self._updateHistory(board, message.data)
        except HistoryError, e:
            self.log(ERROR, " ".join([str(a) for a in e.args]))
            self.boards[board]["history"] = []
            self.boards[board]["send_from_start"] = True
            return False
        return True


    def _updateHistory(self, board, data):
        """
        Internal method
        Update the board history by analyzing a DGT_MSG_BUS_UPDATE message data.
        Raise an HistoryError if something goes wrong.
        """
        while len(data) != 0:
            ## From dgtbrd.h :
            #On the recognition of the first byte of a message: The above definitions
            #imply that the first byte of a message is always ranged
            #from 40-5f for a field change message, 60-69 or 70-79 for a time message,
            #and 6a to 6f, or 7a to 7f for a 1-byte message. 
            #(all values hexadecimal)
            if 0x40 <= data[0] <= 0x5f:
                ## From dgtbrd.h :
                #On every detected change of piece positions this change is written to EEPROM
                #in a 2-byte message, which cover exactly the same data as is sent to the PC
                #in the UPDATE_BOARD mode.
                #The formatting of the 2-byte piece update tag is:
                #First byte:     0t0r nnnn (n is piece code, see before)
                #			  (piece code EMPTY when a piece is removed)
                #			  (t is recognition bit, always 1)
                #			  (r is reserved)
                #		so the first byte of this tag is always in the 
                #		range 0x40 to 0x5f
                #Second byte:    00ii iiii (i = 0 to 63 (0x3f), the field number as
                #			   defined before)
                #
                if len(data) < 2:
                    raise HistoryError("HistoryError: The last square update is not 2-byte long")
                if not 0 <= data[1] < 64:
                    raise HistoryError("HistoryError: The second byte of a square update is not in range 0-63")
                x, y = data[1] % 8, (7 - data[1]//8)
                self.boards[board]["history"].append( ["square", x, y, PIECE_CODE[data[0] & 0x0F]] )
                data = data[2:]
            elif 0x60 <= data[0] <= 0x69 or 0x70 <= data[0] <= 0x79:
                ## From dgtbrd.h :
                #On the pressing of the clock, the time of the halted clock is written in
                #a time message. It might be that when the moves are done very fast, the
                #storage is skipped.
                #
                #Format of a three-byte time message:
                #First byte:    0uuf hhhh (f=1 for time in left clock screen, seen from
                #			  the front)
                #			 ( hhhh: hours, valued 0 to 9)
                #			 (uu recognition bits, both 1, so byte 0 has the
                #			 ( value range of 0x60 to 0x69, or 0x70 to 0x79)
                #Second byte:   0MMM mmmm (MMM: Tens of minutes (range 0 to 5),
                #			 (mmmm: minute units, range 0 to 9)
                #
                #Third byte:    0SSS ssss (SSS: tens of seconds, range 0-5)
                #			 (ssss: seconds units, range 0-9)
                #
                if len(data) < 3:
                    raise HistoryError("HistoryError: The last clock update is not 3-byte long")
                clock = ["clock"]
                clock.append(data[0] & 0x0F) # Hours
                clock.append( ((data[1] & 0xF0)>>4)*10 + (data[1] & 0x0F) ) # Minutes
                clock.append( ((data[2] & 0xF0)>>4)*10 + (data[2] & 0x0F) ) # Seconds
                clock.append(data[0] & 0x10 == 0 and CLOCK_LEFT or CLOCK_RIGHT) # Halted clock
                self.boards[board]["history"].append(clock)
                data = data[3:]
            elif 0x6a <= data[0] <= 0x6f or 0x7a <= data[0] <= 0x7f:
                if data[0] == EE_NOP:
                    pass
                elif data[0] == EE_POWERUP:
                    #- At power-on, three tags EE_NOP are written, followed by a one-byte EE_POWERUP message. 
                    #After this, an UPDATE_BOARD message is written (in virtually random sequence)
                    #for every piece that is found on the board, at power-on.
                    self.log(ERROR, "A power-up occurred on board %s. A lot of unwanted updates will arrive", board)
                elif data[0] == EE_WATCHDOG_ACTION:
                    #When the board is equipped with a watchdog timer, and the watchdog times out,
                    #an EE_WATCHDOG_ACTION is written and after that, the above described power-up 
                    #procedure takes place.
                    self.log(ERROR, "A watchdog action occurred on board %s. A lot of unwanted updates will arrive", board)
                elif data[0] == EE_BEGINPOS:
                    #- When at any time a normal starting position for chess is found, with the 
                    #player for white having the board connector on his left hand, an EE_BEGINPOS tag
                    #is written, and an EE_BEGINPOS_ROT tag is written when white has the
                    #connector at his right hand (rotated)
                    self.boards[board]["history"].append( ["begin_position"] )
                elif data[0] == EE_BEGINPOS_ROT:
                    self.boards[board]["history"].append( ["begin_position_rotated"] )
                elif data[0] == EE_FOURROWS:
                    #- When 16 chess figures are found on the board, all in the A, B, G and H row,
                    #which are not(!) in a normal chess starting position, the one-byte
                    #EE_FOURROWS message is written, to be tolerant on erroneous placement and i.e.  
                    #to be able to play the "Random Chess" as proposed by Bobby Fischer.
                    self.boards[board]["history"].append( ["four_rows"] )
                elif data[0] == EE_EMPTYBOARD:
                    #When an empty board is detected, the one-byte EE_EMPTYBOARD message is
                    #written.
                    self.boards[board]["history"].append( ["empty"] )
                elif data[0] == EE_DOWNLOADED:
                    #When the data of the internal storage are sent upon reception of the
                    #DGT_SEND_EE_MOVES command, the one-byte EE_DOWNLOADED message is sent
                    # XXX error if it is not in a DGT_BUS_SEND_FROM_START
                    self.log(DEBUG, "EE_DOWNLOADED")
		    pass
                elif data[0] == EE_START_TAG:
                    self.log(WARNING, "Found a start tag, reseting history of board %s", board)
                    self.boards[board]["history"] = []
                elif data[0] in (EE_EOF, EE_FUTURE_1, EE_FUTURE_2):
                    pass
                else:
                    raise HistoryError("HistoryError: Unknown one-byte update: %s", data[0])
                data.pop(0)
            else:
                raise HistoryError("HistoryError: Unknown update: %s", data[0])


    def _sendOrder(self, msg, port=None):
        """
        Send a formatted message, adding the checksum.
        msg must be a bytearray. It is altered with the checksum
        if port is None, send on all open ports
        """
        ## From dgtbrd.h :
        #byte 4: checksum: this is the sum of all bytes from start of the message
        #    upto the last byte before the checksum. (D7 always 0)
        #    I.e. message code 0x81 0x10 0x06 will carry checksum byte 0x17

        if port is None:
            for port in [p for p in self.ports.values() if p["handler"] is not None]:
                self._sendOrder(msg, port)
            return
        checksum = sum(msg) ^ 0x80 # Force D7 to 0
        msg.append(checksum)
        if port["handler"] is None:
            raise Exception("Port %s is not open" % port["name"])
        port["handler"].flushInput()
        self.log(DEBUG, "Sending message on port %s : %s", port["name"], byteToHex(msg))
        written_bytes = port["handler"].write(msg)
        if written_bytes != len(msg):
            raise Exception("Written bytes differ from len(message)")


    def _receiveMessage(self, port, timeout=0.1): # 0.1 is arbitrary - dgtbrd.h says 80 ms
        """
        Build a message list from serial connection
        Return a validated BoardMessage list, empty if a problem occurred
        Raise a ChecksumError in case of the answer has a wrong checksum
        """
        self.log(DEBUG, "Reading on port %s", port["name"])
        port["handler"].timeout = timeout
        msg_list = []
        bytes_buffer = bytearray()
        cont = True
        while cont:
            read_bytes = bytearray(port["handler"].read(100))
            bytes_buffer += read_bytes
            if len(read_bytes) == 0:
                cont = False

        self.log(DEBUG, "Received bytes from %s : %s", port["name"], byteToHex(bytes_buffer))
        old_len = 0
        message = BoardMessage()
        while len(bytes_buffer) not in (0, old_len):
            old_len = len(bytes_buffer)
            rejected, bytes_buffer = message.append(bytes_buffer)
            if len(rejected) != 0:
                self.log(WARNING, "Rejected bytes (between messages) : %s", byteToHex(rejected))
            if message.getState() == "complete":
                msg_list.append(message)
                self.log(DEBUG, "Built message : %s", byteToHex(message.buf))
                message = BoardMessage()
            elif message.getState() == "error":
                self.log(WARNING, "Incorrect checksum on message %s", byteToHex(message.buf))
                raise ChecksumError("Incorrect checksum on message %s" % byteToHex(message.buf))
        if len(bytes_buffer) != 0:
            self.log(WARNING, "Not all bytes were used to build messages")
        if len(msg_list) == 0:
            self.log(WARNING, "Unable to build a correct message from port %s", port["name"])
        return msg_list


    def _sendToBoard(self, board, msg):
        """
        Send a message to the given board.
        msg must be a single byte defined by DGT (see start of this file)
        """
        ## From dgtbrd.h :
        #All commands DGT_BUS_xx have the following format:
        #byte 1: command, i.e. DGT_BUS_SEND_BDR (D7 always 1)
        #byte 2: MSB of addressed board (D7 always 0)
        #byte 3: LSB of addressed board (D7 always 0)
        address = self.boards[board]["bus_address"]
        port = self.ports[self.boards[board]["port_name"]]
        self._sendOrder((bytearray([msg])+address), port)

    def _receiveFromBoard(self, board, timeout=0.1): # 0.1 is arbitrary - dgtbrd.h says 80 ms
        """
        Build a message list from serial connection on 
        the port on which the board is
        """
        port = self.ports[self.boards[board]["port_name"]]
        return self._receiveMessage(port, timeout)

    def _communicateWithBoard(self, board, order, expected_message, tries=3, timeout=0.1):  # 0.1 is arbitrary
        """
        Send a message to the given board and verify the answer.
        Manage retries due to checksum errors
        Return the answer itself
        Raise a ChecksumError in case of the answer has a wrong checksum and tries == 1
        """
        board_dict = self.boards.get(board, None)
        if board_dict is None:
            raise Exception("Board %s is unknown" % board)
        bus_address, port_name = board_dict["bus_address"], board_dict["port_name"]
        for i in range(tries):
            self.log(VERBOSE, "Sending order %s to board %s (try No %s)" % (order, board, i+1))
            self._sendToBoard(board, order)
            try:
                for message in self._receiveFromBoard(board, timeout):
                    if message.msg_type == expected_message:
                        if message.board_address == bus_address:
                            return message
                        self.log(WARNING, "Another board responded to the order %s !" % order)
                    else:
                        self.log(WARNING, "Received unwanted message : %s", byteToHex(message.buf))
            except ChecksumError, e:
                if tries==1:
                    raise
        self.log(ERROR, "No response from board %s" % board)



if __name__ == "__main__":
    port = "COM1"
    #port = "/dev/ttyS0"
    b = BoardTranslator()
    b.usePort(port)

    print "---------------- NEW SCAN ---------------------"
    b.discoverBoards()
    boards = b.getBoards()
    print "Discovered boards : %s" % boards

    print "\n----------BOARD POSITION AND CLOCK-------------"
    for serial, port in boards:
        print "  ---> Board %s" % serial
        print b.getBoardPosition(serial)
        print b.getBoardClock(serial)


    print b.getBoardHistory(6685)

    #def setBoardStart(self, board):
    #def getBoardHistory(self, board, history_from=0):


