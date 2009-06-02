-- | Serial provides line-oriented access to serial ports on POSIX
-- compatible systems.  If you need low level access,
-- character-at-a-time reading and writing, or other such
-- procedures, you need to access the serial port directly; this
-- library won't help you.  However, most devices hanging off of
-- serial ports today work by reading and writing commands.  In many
-- cases, commands are non-blocking and you can send additional
-- commands before you receive the response to the last one.
-- "System.Serial.SerialManager" provides a wrapper around this
-- access which tries to match up responses to waiting functions
-- which have called it.
-- 
-- The only function here is 'openSerial', since thereafter the normal
-- functions from "System.IO" such as 'hClose', 'hGetLine', and
-- 'hPutStr' work normally.  Just be sure you send the right end of
-- line sequence for your hardware!  Some devices want CR-LF, others
-- just LF, others just CR, and they may return their results using a
-- different end of line than they accept.

module System.Serial (openSerial, StopBits(One,Two), Parity(Even,Odd,NoParity), FlowControl(Software,NoFlowControl)) where

import System.Posix.IO
import System.Posix.Terminal
import Control.Monad
import System.Posix.Types
import System.IO

-- | "Serial" lets the user set the number of stop bits, the parity,
-- flow control (there is no hardware flow control, since it isn't
-- supported in the "System.Posix.IO" library), number of bits per
-- byte, and the baud rate.  The baud rate is declared by the
-- 'BaudRate' in "System.Posix.Terminal".  'StopBits', 'Parity', and
-- 'FlowControl' are defined here.
data StopBits = One | Two
data Parity = Even | Odd | NoParity
data FlowControl = Software | NoFlowControl


-- | 'openSerial' opens the serial port and sets the options the user
-- passes, makes its buffering line oriented, and returns the handle
-- to control it.  For example, an Olympus IX-81 microscope attached
-- to the first serial port on Linux would be opened with
-- 
-- > openSerial "/dev/ttyS0" B19200 8 One Even Software
-- 
openSerial :: String       -- ^ The filename of the serial port, such as @/dev/ttyS0@
           -> BaudRate     
           -> Int          -- ^ The number of bits per word, typically 8
           -> StopBits     -- ^ Almost always @One@ unless you're talking to a printer
           -> Parity       
           -> FlowControl
           -> IO Handle
openSerial dev baud bitsPerByte stopBits parity flow = do
  fd <- openFd dev ReadWrite Nothing 
        OpenFileFlags { append = True,
                        exclusive = False,
                        noctty = True,
                        nonBlock = False,
                        trunc = False }
  termOpts <- getTerminalAttributes fd
  let termOpts' = configureSettings termOpts baud 
                  bitsPerByte stopBits parity flow
  setTerminalAttributes fd termOpts' Immediately
  h <- fdToHandle fd
  hSetBuffering h LineBuffering
  return h


-- System.Posix.Terminal defines a couple of commands to set options
-- in the TerminalAttributes type (which maps onto a C struct).  The
-- rest are set through a rather awkward enterface, which I wrap here
-- in easier to use commands.

withParity termOpts Even = termOpts `withMode` EnableParity 
                          `withoutMode` OddParity
withParity termOpts Odd = termOpts `withMode` EnableParity
                         `withMode` OddParity
withParity termOpts NoParity = termOpts `withoutMode` EnableParity

withFlowControl termOpts NoFlowControl = termOpts
                                         `withoutMode` StartStopInput
                                         `withoutMode` StartStopOutput
withFlowControl termOpts Software = termOpts
                                    `withMode` StartStopInput
                                    `withMode` StartStopOutput

withStopBits termOpts One = termOpts `withoutMode` TwoStopBits
withStopBits termOpts Two = termOpts `withMode` TwoStopBits

-- All these are now used in a function which properly sets up the
-- serial port properties in the struct.

configureSettings termOpts baud bitsPerByte stopBits parity flow =
    termOpts `withInputSpeed` baud
                 `withOutputSpeed` baud
                 `withBits` bitsPerByte
                 `withStopBits` stopBits
                 `withParity` parity
                 `withFlowControl` flow
                 `withMode` EchoErase
                 `withMode` EchoKill
                 `withMode` ProcessInput

