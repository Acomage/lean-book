module
import LeanBook.example

public def main : IO Unit := do
  let resultHtml : Html := Handle.handle myBook
  let outStr := "<!DOCTYPE html>\n" ++ resultHtml.render
  IO.println outStr
