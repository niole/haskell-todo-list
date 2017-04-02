{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified Data.Text as DT
import Data.Text (Text, snoc)

{--
        add todos
        remove todos
        extra: mark todo as completed
--}

data State = State
        { inputString :: Text,
          todos :: [(Int, Text)]
        }

{--
        renders a todo item
--}
renderTodo :: (Int, Text) -> Picture
renderTodo (index, content) = (rectangle 2.0 1.0) & (text content)

{--
       renders the input with what has been typed so far
--}

renderInput :: Text -> Picture
renderInput input = text input

{--
       handles backspaces and updates state accordingly
--}
deleteChar :: State -> State
deleteChar (State inputString todos) = State (DT.init inputString) todos

{--
       handles characters and spaces, updates state accordingly
--}
addChar :: Char -> State -> State
addChar chr (State inputString todos) = State (snoc inputString chr) todos

{--
       handles enter
--}

{--
       handles clicks
--}

{--
       handles key presses
--}

{--
       handles clicks
--}

{--
       removes clicked todo item
--}

main :: IO()
main = putStrLn ""
