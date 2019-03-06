module HNApi exposing (fetchFakeNews)

import Http
import Json.Decode exposing (Decoder, int, list, string)


fetchFakeNews =
    [ { title = "Aerospace News", tag = "science" }, { title = "Programing News is Cool", tag = "compsci" }, { title = "Earth and Neature News", tag = "science" }, { title = "Turing Machine News", tag = "compsci" } ]


