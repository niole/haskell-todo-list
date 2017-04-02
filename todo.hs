{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified Data.Text as DT
import Data.Text (Text, snoc, pack, append)

{--
        add todos
        remove todos
        extra: mark todo as completed
--}

isNonLetter :: Text -> Bool
isNonLetter c = case c of
        "Up" -> True
        "Down" -> True
        "Left" -> True
        "Right" -> True
        "Backspace" -> True
        "Tab" -> True
        "Enter" -> True
        "Shift" -> True
        "Ctrl" -> True
        "Alt" -> True
        "Esc" -> True
        "PageUp" -> True
        "PageDown" -> True
        "End" -> True
        "Home" -> True
        "Insert" -> True
        "Delete" -> True
        "CapsLock" -> True
        "NumLock" -> True
        "ScrollLock" -> True
        "PrintScreen" -> True
        "Break" -> True
        "Separator" -> True
        "Cancel" -> True
        "Help" -> True
        _  -> False

data State = State
        { inputString :: Text,
          todos :: [(Int, Text)]
        }


{--
       handles backspaces and updates state accordingly
--}
deleteChar :: State -> State
deleteChar (State inputString todos) = State (DT.init inputString) todos

{--
       handles characters and spaces, updates state accordingly
--}
addChar :: Text -> State -> State
addChar chr (State inputString todos) = State (append inputString chr) todos

{--
       handles clicks
--}
handleClick :: Event -> State -> State
handleClick (MousePress LeftButton (x, y)) (State content todos) = State content (removeTodo todos $ yToIndex y)
handleClick _ s = s

getRemovableTodoIndex :: Point -> Int
getRemovableTodoIndex (x, y) = yToIndex y

{--
       handles key presses
--}
handleKeyPress :: Event -> State -> State
handleKeyPress (KeyPress which) s
        | isNonLetter which = handleNonLetter which s
        | otherwise = handleLetter which s

{--
        handles events
--}
handleEvent :: Event -> State -> State
handleEvent (KeyPress which) s = handleKeyPress (KeyPress which) s
handleEvent (MousePress button point) s = handleClick (MousePress button point) s
handleEvent _ s = s

{--
       handles letter presses
--}
handleLetter :: Text -> State -> State
handleLetter letter s = addChar letter s

{--
       handles non-letter presses
--}
handleNonLetter :: Text -> State -> State
handleNonLetter "Enter" s = addTodo s
handleNonLetter "Backspace" s = deleteChar s
handleNonLetter _ s = s

{--
       removes todo
--}
removeTodo :: [(Int, Text)] -> Int -> [(Int, Text)]
removeTodo [] index = []
removeTodo ((i, content):todos) index
        | index < 0          = todos
        | index == i         = removeTodo todos index
        | index < i          = (i-1, content):(removeTodo todos index)
        | otherwise          = (i, content):(removeTodo todos index)
        | otherwise          = (i, content):(removeTodo todos index)

{--
       adds todo item by taking text out of input content state
       and adds to front of todos
--}
addTodo :: State -> State
addTodo (State inputContent todos) = State "" ( (0, inputContent):(incTodos todos) )

decTodos :: [(Int, Text)] -> [(Int, Text)]
decTodos todos = map (\((i, content)) -> (i-1, content)) todos

incTodos :: [(Int, Text)] -> [(Int, Text)]
incTodos todos = map (\((i, content)) -> (i+1, content)) todos

renderTodos :: [(Int, Text)] -> Picture
renderTodos todos =  foldl (\a -> \b -> a & (renderTodo b)) blank todos

{--
        renders a todo item
--}
renderTodo :: (Int, Text) -> Picture
renderTodo (index, content) = translated 0.0 (indexToY index)  $ (rectangle 2.0 1.0) & (text content)

indexToY :: Int -> Double
indexToY index = (fromIntegral(index)*(-1.0)) - 1.0

yToIndex :: Double -> Int
yToIndex y = round((y + 1.0)*(-1.0))

{--
       renders the input with what has been typed so far
--}
renderInput :: Text -> Picture
renderInput input = text input

{--
        renders whole todo list
--}
render :: State -> Picture
render (State inputContent todos) = (renderInput inputContent) & (renderTodos todos)

initialState :: State
initialState = State "" []

-- world -> (Double -> world -> world) -> (Event -> world -> world) -> (world -> Picture) -> IO ()
main :: IO()
main = interactionOf initialState (\x -> \s -> s) handleEvent render
