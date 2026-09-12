port module App.Ports exposing (downloadCsv, printPage)


port downloadCsv : ( String, String ) -> Cmd msg


port printPage : () -> Cmd msg
