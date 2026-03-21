module
import LeanBook.example
import LeanBook.handler

public def main : IO Unit := do
  -- 初始状态为空，运行 RenderM
  let (resultHtml, _) := (Handle.handle myBook).run {}
  let outStr := "<!DOCTYPE html>\n" ++ resultHtml.render
  IO.println outStr
