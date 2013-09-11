{-# LANGUAGE OverlappingInstances#-}

{- arch-tag: Python file-like objects
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module     : Python.Objects.File
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable

Python file-like objects

Written by John Goerzen, jgoerzen\@complete.org

This module provides a Haskell interface to work with Python file-like objects.
The Haskell interface is a "System.IO.HVIO" interface, which is similar
in concept to the Python file-like object system.

You can create such objects by using 'openPyFile' from this module, or
'MissingPy.FileArchive.GZip.openGz' or 'MissingPy.FileArchive.BZip2.openBz2'.

Functions that you can use to operate on these objects are defined at
"System.IO.HVIO".
-}

module Python.Objects.File (-- * PyFile Objects
                            PyFile,
                            mkPyFile,
                            fromPyFile,
                            openPyFile,
                            pyfwrap,
                            openModeConv
                      )
where
import Python.Objects (       PyObject(..)
                            , callMethodHs
                            , fromPyObject
                            , getattr
                            , hasattr
                            , noKwParms
                            , noParms
                            , runMethodHs
                            , showPyObject
                            , toPyObject
                            )
import Python.Interpreter (callByName)
import System.IO (IOMode(..), SeekMode(..))
import System.IO.Error (eofErrorType)
import System.IO.Unsafe (unsafeInterleaveIO)
import Python.Exceptions (catchPy, exc2ioerror)
import Foreign.C.Types (CInt, CLong)

{- | The basic type for a Python file or file-like object.

'PyFile's are a member of System.IO.HVIO and can be used as any other
Haskell HVFS object such as a Handle.

'PyFile' objects cannot reliably detect EOF when asked by 'vIsEOF', but
can detect it and raise the appropriate IOError when it is reached.
Also, 'PyFile' objects cannot determine if they are readable, writable,
or seekable in advance.
 -}
newtype PyFile = PyFile PyObject

{- | Takes a 'PyObject' representing a Python file or file-like object
and makes it into a 'PyFile'. -}
mkPyFile :: PyObject -> PyFile
mkPyFile o = PyFile o

{- | Extracts the 'PyObject' representing this 'PyFile'. -}
fromPyFile :: PyFile -> PyObject
fromPyFile (PyFile o) = o

{- | Convert a Haskell open mode to a Python mode string -}
openModeConv ReadMode = "r"
openModeConv WriteMode = "w"
openModeConv AppendMode = "a"
openModeConv ReadWriteMode = "w+"

{- | Open a file on disk and return a 'PyFile'. -}
openPyFile :: FilePath -> IOMode -> IO PyFile
openPyFile fp mode =
           do parms1 <- toPyObject [fp]
              parms2 <- toPyObject [openModeConv mode]
              obj <- callByName "open" [parms1, parms2] []
              return $ mkPyFile obj

{- | Wrap an operation, raising exceptions in the IO monad as appropriate. -}
pyfwrap :: PyFile -> (PyObject -> IO a) -> IO a
pyfwrap (PyFile pyobj) func = catchPy (func pyobj) exc2ioerror
