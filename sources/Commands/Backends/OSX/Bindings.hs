{-# LANGUAGE ViewPatterns #-}
module Commands.Backends.OSX.Bindings where
import Commands.Backends.OSX.Bindings.Raw
import Commands.Backends.OSX.Marshall
import Commands.Backends.OSX.Types

import Foreign.C.String                   (peekCString, withCString)


currentApplication :: IO Application
currentApplication = do -- TODO munge, default to Global
 path <- currentApplicationPath
 return path

-- |
-- TODO Applications whose name/paths have Unicode characters may or may not marshall correctly.
currentApplicationPath :: IO String
currentApplicationPath = objc_currentApplicationPath >>= peekCString

-- |
pressKey :: [Modifier] -> Key -> IO ()
pressKey (encodeModifiers -> flags) (encodeKey -> key) =
 objc_pressKey flags key

-- |
clickMouse :: [Modifier] -> Positive -> MouseButton -> IO ()
clickMouse = undefined
-- clickMouse (MouseClick (encodeModifiers -> flags) (encodePositive -> n) (encodeButton -> button)) = objc_clickMouse

-- |
getClipboard :: IO ClipboardText
getClipboard = objc_getClipboard >>= peekCString

-- |
--
-- note: unlike the keyboard shortcuts of 'copy',
-- contents don't show up in Alfred's clipboard history.
setClipboard :: ClipboardText -> IO ()
setClipboard s = withCString s objc_setClipboard

-- |
openURL :: URL -> IO ()
openURL s = withCString s objc_openURL

-- |
openApplication :: Application -> IO ()
openApplication s = withCString s objc_openApplication

