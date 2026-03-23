module
import LeanBook.example
import LeanBook.handler

/-- Write one HTML file per chapter, an index redirect, and a shared style.css. -/
public def main : IO Unit := do
  let myTheme : Theme := { contentWidth := "68ch" }
  let initialState : RenderState := { rules := generateGlobalCss myTheme }

  let total        := myBook.chapters.length
  let docTitleStr  := inlinesToPlainText myBook.title

  -- Render chapters sequentially, threading the CSS-accumulation state
  let mut state := initialState
  for (i, ch) in myBook.chapters.enum do
    let chNum := i + 1
    let (pageHtml, newState) :=
      (renderChapterPage docTitleStr chNum total ch).run state
    state := newState
    IO.FS.writeFile s!"chapter-{chNum}.html"
      ("<!DOCTYPE html>\n" ++ pageHtml.render)

  -- Minimal index.html that redirects to the first chapter
  IO.FS.writeFile "index.html" <|
    "<!DOCTYPE html>\n" ++
    "<html lang=\"en\"><head><meta charset=\"UTF-8\">" ++
    "<meta http-equiv=\"refresh\" content=\"0; url=chapter-1.html\">" ++
    "<title>" ++ docTitleStr ++ "</title></head>" ++
    "<body><p><a href=\"chapter-1.html\">Go to book</a></p></body></html>"

  -- Write accumulated CSS
  IO.FS.writeFile "style.css" (renderCss state.rules)

  IO.println s!"Generated {total} chapter(s), index.html, and style.css."
