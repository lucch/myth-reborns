import Control.Applicative
import Control.Concurrent
import Control.Monad
import Graphics.Gloss.Data.Color as Gloss
import Graphics.Gloss.Data.Picture as Gloss
import Graphics.Gloss.Rendering as GlossR
import qualified Graphics.UI.GLUT as GLUT
--import Graphics.Rendering.OpenGL as OpenGL hiding (Error)
import Graphics.UI.GLFW as GLFW
import System.Exit
import System.IO

type Position = Point

main :: IO ()
main = do
    -- Without line below, program breaks on Ubuntu! :-(
    (_,_) <- GLUT.getArgsAndInitialize
    setErrorCallback (Just errorCallback)
    initialized <- GLFW.init
    unless (not initialized) $ do
        maybeWin <- createWindow 640 480 "The Myth Reborns: A Haskell Game" Nothing Nothing
        case maybeWin of
            (Just win) -> do
                makeContextCurrent maybeWin
                --setKeyCallback win (Just keyCallback)
                state <- initState
                loop win state (0.0, -20.0)
            _          -> terminate >> exitFailure

errorCallback :: Error -> String -> IO ()
errorCallback err msg = hPutStrLn stderr $ show err ++ ": " ++ msg

--keyCallback :: Window -> Key -> Int -> KeyState -> ModifierKeys -> IO () 
--keyCallback win Key'Escape _ KeyState'Pressed _ = setWindowShouldClose win True
--keyCallback _ _ _ _ _ = return ()
loop :: Window -> State -> Position -> IO ()
loop win state (x, y) = do
    shouldClose <- windowShouldClose win
    if shouldClose
        then do
            destroyWindow win
            terminate
        else do
            threadDelay 20000
            displayPicture (640, 480) (makeColor 1 1 1 1) state 1.0 $
                pictures [
                    translate (-300.0) 150.0 $ scale 0.5 0.5 $ text "Congratz Jeremito!",
                    translate (-300.0) 100.0 $ scale 0.2 0.2 $ text "(with exclamation mark...)"
                ]
            withModelview (640, 480) $ renderPicture state 1.0 $
                translate x y $ scale 2.0 2.0 $ pictures [
                    color violet $ circleSolid 20.0,
                    translate (-7.0) (6.0) $ color white $ circleSolid 2.5,
                    translate (4.5) (4.5) $ color white $ polygon [(0.0, 0.0), (0.0, 2.5), (7.0, 2.5), (7.0, 0.0)],
                    translate 0.0 (-2.0) $ rotate 180.0 $ color rose $ thickArc 0.0 180.0 8.0 2.0,
                    translate 0.0 12.0 $ color orange $ polygon [(-20.0, 0.0), (0.0, 35.0), (20.0, 0.0)],
                    translate (-25.0) (-35.0) $ scale 0.1 0.1 $ color (dark blue) $ text "j3r3mias"
                ]
            swapBuffers win
            pollEvents
            (l, r, u, d) <- (, , ,) <$> isKeyPressed Key'Left
                                    <*> isKeyPressed Key'Right
                                    <*> isKeyPressed Key'Up
                                    <*> isKeyPressed Key'Down
            let newPos = moveCharacter (x, y) (l, r, u, d)
            loop win state newPos
  where
    isKeyPressed key = ((==) KeyState'Pressed) <$> getKey win key
    
moveCharacter :: Position -> (Bool, Bool, Bool, Bool) -> Position
moveCharacter (x, y) (True, _, _, _) = (x - 10, y)
moveCharacter (x, y) (_, True, _, _) = (x + 10, y)
moveCharacter (x, y) (_, _, True, _) = (x, y + 10)
moveCharacter (x, y) (_, _, _, True) = (x, y - 10)
moveCharacter pos _                  = pos


