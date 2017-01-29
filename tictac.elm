import Html exposing (beginnerProgram, div, button, text)
import Html.Events exposing (onClick)
import Array

main = beginnerProgram { model = model, view = view, update = update }

type alias Model = {
  elements : Array.Array String,
  elementsB : Array.Array String,
  elementsC : Array.Array String,
  state : Int,
  winner : String
}

model : Model
model = Model (Array.fromList ["","",""]) (Array.fromList ["","",""]) (Array.fromList ["","",""]) 0 "You can play" 

type Msg = Checking Int Int | Load


update msg model =
  case msg of     
    Checking x y ->
      if (check model x y) == True  && model.winner == "You can play" then
        if x == 0 then   
          if model.state == 0 then
            {model| elements = Array.set y "X" model.elements,
             state = 1,
              winner = (whowin model "X")
             }  
          else
            {model| elements = Array.set y "O" model.elements,
                    state = 0,
                    winner = (whowin model "O")
                    }
        else if x == 1 then
         if model.state == 0 then
            {model| elementsB = Array.set y "X" model.elementsB,
                    state = 1,
                    winner = (whowin model "X")
            }
          else
            {model| elementsB = Array.set y "O" model.elementsB,
                    state = 0,
                    winner = (whowin model "O")
                    
             }
        else if x == 2 then
          if model.state == 0 then
            {model| elementsC = Array.set y "X" model.elementsC,
                    state = 1,
                    winner = (whowin model "X")
            }
          else
            {model| elementsC = Array.set y "O" model.elementsC,
                    state = 0,
                    winner = (whowin model "O")
            }
        else
          model
       else
         model

    Load ->
      {model| elements = (Array.fromList ["","",""]),
             elementsB = (Array.fromList ["","",""]),
             elementsC = (Array.fromList ["","",""]),
             state = 0,
             winner = "You can play"
      }
     





display : Model -> Int -> Int -> String
display model x y = 
  if x == 0 then
    if Array.get y model.elements == Just "X" then
      "X"
    else if Array.get y model.elements == Just "O" then
      "O"
    else
      ""
  else if x == 1 then
    if Array.get y model.elementsB == Just "X" then
      "X"
    else if Array.get y model.elementsB == Just "O" then
      "O"
    else
      ""
  else if x == 2 then
    if Array.get y model.elementsC == Just "X" then
      "X"
    else if Array.get y model.elementsC == Just "O" then
      "O"
    else
      ""
  else
    ""

whowin : Model -> String -> String
whowin model x =
  if (display model 0 0) == x && (display model 0 1) == x && (display model 0 2) == x  then
    (String.append "the winner is " x)
  else if (display model 1 0) == x && (display model 1 1) == x && (display model 1 2) == x  then
    (String.append "the winner is " x)
  else if (display model 2 0) == x && (display model 2 1) == x && (display model 2 2) == x  then
    (String.append "the winner is " x)
  else if (display model 0 0) == x && (display model 1 0) == x && (display model 2 0) == x  then
    (String.append "the winner is " x)
  else if (display model 0 1) == x && (display model 1 1) == x && (display model 2 1) == x  then  
    (String.append "the winner is " x)
  else if (display model 0 2) == x && (display model 1 2) == x && (display model 2 2) == x  then
    (String.append "the winner is " x)
  else if (display model 0 0) == x && (display model 1 1) == x && (display model 2 2) == x  then
    (String.append "the winner is " x)
  else if (display model 0 2) == x && (display model 1 1) == x && (display model 2 0) == x  then
    (String.append "the winner is " x)
  else
    "You can play"
    
check : Model -> Int -> Int -> Bool
check model x y =
  if x == 0 then
    if Array.get y model.elements == Just "" then
      True
    else
      False
  else if x == 1 then
    if Array.get y model.elementsB == Just "" then
     True
    else
     False
  else if x == 2 then
    if Array.get y model.elementsC == Just "" then
     True
    else
     False
   else
     False

view model =
  div []
    [ 
     div[] [ --(display model 0 0)
      button [ onClick (Checking 0 0) ] [ text  (display model 0 0) ],
      button [ onClick (Checking 0 1) ] [ text (display model 0 1) ],
      button [ onClick (Checking 0 2) ] [ text (display model 0 2) ]
     ],
     div[] [ 
      button [ onClick (Checking 1 0) ] [ text (display model 1 0) ],
      button [ onClick (Checking 1 1) ] [ text (display model 1 1) ],
      button [ onClick (Checking 1 2) ] [ text (display model 1 2) ]
     ],
     div[] [ 
      button [ onClick (Checking 2 0) ] [ text (display model 2 0) ],
      button [ onClick (Checking 2 1) ] [ text (display model 2 1) ],
      button [ onClick (Checking 2 2) ] [ text (display model 2 2) ]
     ],
     div [] [ text (toString model.winner) ],
     button [ onClick (Load) ] [ text "Restart the party" ]

    ]
